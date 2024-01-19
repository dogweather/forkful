---
title:                "Analisi del html"
html_title:           "Arduino: Analisi del html"
simple_title:         "Analisi del html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Che cosa è e Perché?

L'analisi (Parsing) HTML è la procedura mediante la quale una stringa di codice HTML viene convertita in un formato leggibile dalla macchina. E' molto utilizzata per estrarre informazioni da pagine web professionalmente e comodamente.

## Come Fare:

Arduino non supporta direttamente l'analisi HTML, ma possiamo utilizzare una libreria esterna come ArduinoJson per farlo. Ecco un esempio di base:

```Arduino
#include "ArduinoJson.h"

void setup(){
  Serial.begin(9600);
  StaticJsonDocument<200> doc;  

  char json[] = "<html><body>Ciao Mondo!</body></html>";
  
  DeserializationError error = deserializeJson(doc, json);
  
  if(error){
    Serial.println("Fallimento Deserializzazione");
  }
  else{
    const char* messaggio = doc["html"]["body"]; 
    Serial.println(messaggio); 
  }
}

void loop(){}
```

Nel Monitor Seriale, vedrai: `Ciao Mondo!`

## Approfondimento:

(1) L'analisi di HTML è antica quanto il web stesso, dato che è la tecnica principale utilizzata dai browser per interpretare i siti web.

(2) Ci sono alternative come BeautifulSoup in Python, ma Arduino richiede soluzioni leggere e veloci, quindi l'utilizzo della libreria ArduinoJson è molto indicata.

(3) L'implementazione dettagliata dell'analisi HTML può variare a seconda del formato e della complessità dell'HTML di input. Tuttavia, il concetto di base rimane sempre lo stesso: leggere l'HTML come stringa e analizzarlo per estrarre le informazioni desiderate.

## Vedi Anche:

Per ulteriori informazioni, consulta le seguenti risorse:

1. [Documentazione ArduinoJson](https://arduinojson.org/)
2. [Tutorial parsing JSON con Arduino](https://learn.adafruit.com/adafruit-huzzah-esp8266-breakout/using-arduino-ide)
3. [Guida alla sintassi JSON](https://www.w3schools.com/js/js_json_syntax.asp)