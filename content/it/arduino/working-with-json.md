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

## Perché

Perché vorresti impegnarti a lavorare con JSON? Semplicemente perché è un formato di dati molto flessibile e ampiamente utilizzato che consente di scambiare informazioni tra diverse piattaforme e linguaggi di programmazione. Inoltre, su Arduino è possibile gestire facilmente e manipolare i dati JSON grazie alla sua libreria integrata. 

## Come fare 
Per lavorare con JSON su Arduino, è necessario utilizzare la libreria ArduinoJSON, disponibile su GitHub. Prima di utilizzarla, è importante scaricarla e installarla seguendo le istruzioni riportate nel suo repository. Una volta installata, abbiamo bisogno di includere la libreria nel nostro sketch, per farlo basta aggiungere la seguente riga all'inizio dello sketch: 

```Arduino
#include <ArduinoJSON.h>
```

Una volta inclusa la libreria, possiamo iniziare a creare i nostri oggetti e array JSON utilizzando la sua sintassi specifica. Ad esempio, per creare un oggetto JSON con due campi "nome" e "cognome" con i relativi valori, possiamo scrivere: 

```Arduino
// Creazione dell'oggetto JSON
StaticJsonDocument<200> doc; // 200 è la dimensione del buffer

// Aggiunta dei campi con i relativi valori
doc["nome"] = "Mario";
doc["cognome"] = "Rossi";

// Serializzazione dell'oggetto in una stringa JSON
char jsonString[200];
serializeJson(doc, jsonString);
```
In questo modo, abbiamo creato un oggetto JSON e lo abbiamo serializzato in una stringa. Ovviamente possiamo creare oggetti più complessi con più campi e valori, e utilizzare anche array per gestire più valori.

Una volta creato l'oggetto JSON, possiamo utilizzarlo per scambiare informazioni tra il nostro Arduino e altri dispositivi come ad esempio uno smartphone o un server. Inoltre, con la libreria ArduinoJSON possiamo anche analizzare e manipolare dati JSON ricevuti dall'esterno. Ad esempio, possiamo leggere il valore di un campo o aggiungere nuovi valori all'oggetto.

## Approfondimento

La libreria ArduinoJSON utilizza una sintassi simile a quella di C++, quindi se sei già familiare con questo linguaggio di programmazione, sarà molto facile utilizzarla. Inoltre, la libreria è ottimizzata per funzionare su dispositivi con risorse limitate come Arduino, quindi puoi gestire anche grandi quantità di dati JSON senza problemi di memoria.

Inoltre, la libreria supporta anche la compatibilità con altri formati di dati come XML e MessagePack, rendendola ancora più versatile e utile per progetti di Internet of Things.

## Vedi anche

- [Repository della libreria ArduinoJSON su GitHub](https://github.com/bblanchon/ArduinoJson)
- [Documentazione ufficiale della libreria ArduinoJSON](https://arduinojson.org/)
- [Articolo su come utilizzare JSON su Arduino su Maker Pro](https://maker.pro/arduino/projects/using-json-on-arduino)