# Cargamos las bibliotecas que nos hacen falta
library(lunar)
library(suncalc)


# Functión: nightjaring.date_btw
# Descripcpión: Determina si una fecha se encuentra dentro de un periodo
# Parámetros:
#   questionDate: La fecha que queremos verificar
#   beginDate:    La fecha de inicio del periodo
#   endDate:      La fecha de fin del periodo
# Devuelve: Booleano
nightjaring.date_btw <- function(questionDate, beginDate, endDate){
  
  return(as.numeric(questionDate) >= as.numeric(beginDate) &&
     as.numeric(questionDate) <= as.numeric(endDate) )
}

# Función: nightjaring.acu_moon
# Descripción: Calcula la luna acumulada entre dos fechas 
# Parámetros:
#   firstDate:  fecha de primera noche (tipo Date)
#   lastDate:   fecha de última noche (tipo Date)
#   shift:      Horas de diferencia respecto a UTC 0
#   moonNight:  Si se desea corregir con las horas de luna nocturnas
# Devuelve: 
#   Luna acumulada entre las dos fechas (double)lun
nightjaring.acu_moon <- function(firstDate, lastDate, shift = 0, moonNight = FALSE, lat = 0, lon = 0){
  start <- firstDate
  end <- lastDate
  
  currentDate <- start
  
  totalMoon = 0;
  
  while(currentDate < end){
    
    illumination <- lunar.illumination(currentDate,shift) 
    if(moonNight){
      correction <- nightjaring.moon_night_overlap(currentDate, lat, lon)[['nightMoon']]
      illumination <- illumination * correction
    }
    totalMoon <- totalMoon + illumination
    currentDate <- currentDate + 1
  }
  return(totalMoon)
}

# Función: nightjaring.mean_moon
# Descripción: Devuelve la cantidad de luna acumulada promedio entre dos fechas, por día
#              Por ejemplo, (un día de luna al 100% + un día de luna al 98%) = luna al 99% de promedio
# Parámetros:
#   firstDate:   fecha de primera noche (tipo Date)
#   lastDate:    fecha de última noche (tipo Date)
#   shift:      Horas de diferencia respecto a UTC 0
nightjaring.mean_moon <- function(firstDate, lastDate, shift = 0){
  start <- firstDate
  end <- lastDate
  
  currentDate <- start
  
  totalMoon = 0;
  totalNights = 0;
  
  while(currentDate < end){
    
    illumination <- lunar.illumination(currentDate, 0) 
    totalMoon <- totalMoon + illumination
    currentDate <- currentDate + 1
    totalNights <- totalNights + 1
  }
  
  meanMoon <- totalMoon / totalNights
  return(meanMoon)
}
# Función: nightjaring.moon_night_overlap
nightjaring.moon_night_overlap <- function(date, lat, lon){

  nextDate <- date + 1  
  moonTimes <- getMoonTimes(date, lat, lon)
  sunTimes <- getSunlightTimes(date, lat, lon)
  moonTimesNext <- getMoonTimes(nextDate, lat, lon)
  sunTimesNext <- getSunlightTimes(nextDate, lat, lon)
  
  

  # Posibilidades:
  
  # 1) Tengo sólo un evento moonSet||moonNextSet entre sunSet y sunNextRise
  # 2) Tengo sólo un evento moonRise||moonNextRise entre sunSet y sunNextRise
  # 3) Tengo un evento moonRise y moonNextSet  entre sunSet y sunNextRise
  # 4) Tengo un event moonSet y un moonNextRise entre sunSet y sunNextRise

  # En cada caso tenemos que computar de manera distinta la cantidad de luna coincidente
  
  # Lo que marca es la noche (el sol), que es nuestra referencia de tiempo
  # ¿Tenemos la luna fuera al empezar la noche del día concreto?
  
  overlapDuration <- 0
  moonDuration <- 0
  moonRise <- 0
  moonSet <- 0
  
  moonSunset <- getMoonPosition(sunTimes[['sunset']], lat, lon)
  moonNextSunrise <- getMoonPosition(sunTimesNext[['sunrise']], lat, lon)
  
  # ¿Está la luna fuera al atardecer?
  if(moonSunset['altitude'] > 0 ){
    
    #Sumamos la luna desde el atardecer hasta que se ponga
    if(!is.na(moonTimes['set']) && moonTimes['set'] > sunTimes['sunset'] ){
      # Sumamos desde el atardecer hasta la puesta de luna, si se pone en el mismo día
      overlapDuration <- as.numeric(moonTimes['set']) - as.numeric(sunTimes['sunset'])
    }
    else{
      
      #La luna está fuera hasta antes del amanecer
      if(!is.na(moonTimesNext['set']) && moonTimesNext['set'] < sunTimesNext['sunset']){
        overlapDuration <- as.numeric(moonTimesNext['set']) - as.numeric(sunTimes['sunset'])
      }
      else{
        # La luna está fuera durante toda la noche
        overlapDuration <- as.numeric(sunTimesNext['sunrise']) - as.numeric(sunTimes['sunset'])
      }
    }
    
  }
  
  # ¿Está la luna fuera al amanecer del siguiente día?
  if(moonNextSunrise['altitude'] > 0){
    
    #Sumamos la luna desde que sale hasta el amanecer
    if(!is.na(moonTimes['rise']) && nightjaring.date_btw(moonTimes[['rise']], sunTimes[['sunset']], sunTimesNext[['sunrise']]) ){
      # Sumamos desde la salida de luna hasta el amanecer
      overlapDuration <-  as.numeric(sunTimesNext['sunrise']) - as.numeric(moonTimes['rise'])
    }
    else{
      if(!is.na(moonTimesNext['rise']) && moonTimesNext['rise'] < sunTimesNext['sunrise']){
        overlapDuration <- as.numeric(sunTimesNext['sunrise']) - as.numeric(moonTimesNext['rise'])
      }
      else{
        # La luna está fuera toda la noche, pero ya lo hemos tenido en cuenta antes
      }
    }
    
  }
  
  # La luna sale y se pone en la misma noche
  
  if(moonSunset['altitude'] < 0 && moonNextSunrise['altitude'] < 0){
    
    if(nightjaring.date_btw(moonTimes['rise'], sunTimes['sunset'], sunTimesNext['sunrise'])){
      moonRise <- moonTimes['rise']
    }
    else if(nightjaring.date_btw(moonTimesNext['rise'], sunTimes['sunset'], sunTimesNext['sunrise'])){
      moonRise <- moonTimesNext['rise']
    }
    
    moonSet <- 0
    
    if(nightjaring.date_btw(moonTimes['set'], moonRise, sunTimesNext['sunrise'])){
      moonSet <- moonTimes['set']
    }
    else if(nightjaring.date_btw(moonTimesNext['set'], moonRise, sunTimesNext['sunrise'])){
      moonSet <- moonTimesNext['set']
    }
    
    
    overlapDuration <- as.numeric(moonSet) - as.numeric(moonRise)
    moonDuration <- overlapDuration
  }
  
  if(moonDuration == 0){
    
    if(!is.na(moonTimes['rise'])){
        
  		if(!is.na(moonTimes['set']) &&  moonTimes['set'] > moonTimes['rise']){
			  
        moonDuration <- as.numeric(moonTimes['set']) - as.numeric(moonTimes['rise'])
    		
  		}
      else{
        moonDuration <- as.numeric(moonTimesNext['set']) - as.numeric(moonTimes['rise'])
      }
    }
    else{
      prevDate <- date -1
      moonTimesPrev <- getMoonTimes(prevDate, lat, lon)
      moonDuration <- as.numeric(moonTimes['set']) - as.numeric(moonTimesPrev['rise'])
    }
  }
  
  
  

  nightDuration <- as.numeric(sunTimesNext['sunrise']) - as.numeric(sunTimes['sunset'])
  
 
  
  nightMoon <- as.numeric(overlapDuration) / as.numeric(moonDuration)
  
  result <- list("moonDuration" = moonDuration, "nightDuration" = nightDuration, "overlapDuration" = overlapDuration, "nightMoon" = nightMoon)

  return(result)
  
}

# Luna llena: lunar.illumination(as.Date('2020-07-05'), 2.50)
# Luna llena: lunar.illumination(as.Date('2020-08-03'), 15.2)


# Ejemplo de uso:


lat = 37.3449745
lon = -6.0634118


# Sencillo, luna media acumulada entre dos fechas:

nightjaring.mean_moon(as.Date('2020-07-01'), as.Date('2020-07-31'))


# Sencillo, directamente entre dos fechas:

nightjaring.acu_moon(as.Date('2020-07-01'), as.Date('2020-07-31'))

# Sencillo, pero ajustando con la duración de luna nocturna

nightjaring.acu_moon(as.Date('2020-07-01'), as.Date('2020-07-31'), TRUE, lat, lon)

# Complejo, directamente un data range, y creando la nueva columna "moon":

individual <- c('331234', '1B50802', '320032')
firstDate <- as.Date(c('2020-06-01', '2020-05-03', '2020-06-25'))
lastDate <- as.Date(c('2020-08-23', '2020-09-10', '2020-07-22'))

nightjars.data <- data.frame(individual, firstDate, lastDate)

nightjars.data['moon'] <- apply(nightjars.data, 1, function(x) nightjaring.acu_moon(as.Date(x['firstDate']), as.Date(x['lastDate']), 1))

# Complejo pero ajustando con la duración de la luna, directamente un data range, y creando la nueva columna "real_moon":

nightjars.data['real_moon'] <- apply(nightjars.data, 1, function(x) nightjaring.acu_moon(as.Date(x['firstDate']), as.Date(x['lastDate']), 1, TRUE, lat, lon))
