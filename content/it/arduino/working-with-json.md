---
title:                "Lavorare con json"
html_title:           "Arduino: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Working with JSON (JavaScript Object Notation) means being able to easily store and interchange data in a structured format. Programmers use JSON because it is lightweight, human-readable, and widely supported by programming languages.

## Come fare:
```
ArduinoJsonArray array = doc.to<ArduinoJsonArray>();
int value = array[0]["key"];
```
L'esempio sopra mostra come leggere un valore da un array JSON utilizzando la libreria ArduinoJson. Il parametro "key" indica la chiave del valore che vogliamo ottenere. Il valore viene quindi memorizzato in una variabile int.


## Approfondimento:
La storia di JSON inizia negli anni '90, in cui veniva utilizzato principalmente come forma di scambio dati tra applicazioni web. Ora è diventato uno standard importante per lo scambio di dati fra diverse piattaforme. Un'alternativa a JSON è XML, ma in confronto JSON è più leggero e più facile da leggere e scrivere. Per utilizzare JSON con Arduino, è necessario installare la libreria ArduinoJson, disponibile su GitHub.

## Vedi anche:
- [Introduzione a JSON](https://www.w3schools.com/js/js_json_intro.asp)
- [Documentazione della libreria ArduinoJson](https://arduinojson.org/)
- [Tutorial su come utilizzare JSON con Arduino](https://randomnerdtutorials.com/decoding-and-encoding-json-with-arduino-or-esp8266/)