---
title:                "Arduino: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui un programmatore potrebbe voler lavorare con JSON, o JavaScript Object Notation. JSON è un formato di dati popolare e flessibile che è diventato lo standard per lo scambio di dati tra applicazioni web e dispositivi embedded, come l'Arduino.

## Come fare
Per iniziare a lavorare con JSON su Arduino, è necessario installare una libreria chiamata ArduinoJSON. Questa libreria può essere facilmente installata tramite il "Library Manager" dell'IDE di Arduino.

Una volta che la libreria è stata installata, è possibile utilizzarla per convertire facilmente i dati in formato JSON e viceversa. Ecco un esempio di codice che mostra come creare un oggetto JSON e successivamente convertirlo in stringa per la trasmissione:

```Arduino
#include <ArduinoJSON.h>

void setup() {
  Serial.begin(9600);

  // Creazione dell'oggetto JSON
  StaticJsonDocument<200> doc;
  doc["nome"] = "Mario";
  doc["cognome"] = "Rossi";
  doc["età"] = 25;

  // Conversione in stringa e invio tramite seriale
  String output;
  serializeJson(doc, output);
  Serial.println(output);
}

void loop() {

}
```

L'output sul monitor seriale sarà il seguente:

`{"nome":"Mario","cognome":"Rossi","età":25}`

## Approfondimenti
Per coloro che desiderano saperne di più su come lavorare con JSON su Arduino, ecco alcuni suggerimenti:

- [Tutorial su come utilizzare ArduinoJSON](https://arduinojson.org/v6/doc/basic-examples/)
- [Documentazione ufficiale di ArduinoJSON](https://arduinojson.org/v6/doc/)
- [Esempi di progetti che utilizzano JSON su Arduino](https://create.arduino.cc/projecthub/projects/tags/json)

## Vedi anche
Ecco altri articoli che potrebbero esserti utili:

- [How To: Utilizzare il modulo WiFi ESP8266 su Arduino](https://www.arduino.cc/en/Tutorial/WiFiWebServer)
- [Come creare una connessione WiFi con Arduino e ESP8266](https://www.arduino.cc/en/Tutorial/WiFiNinaConnect)
- [Come utilizzare la libreria ESP8266WebServer per creare un server web su Arduino](https://www.arduino.cc/en/Tutorial/WiFiWebServer)