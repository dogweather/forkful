---
title:                "Lavorare con i file csv"
html_title:           "Arduino: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il lavorare con CSV (Comma Separated Values) significa manipolare dati in formato tabella, dove ogni campo è separato da una virgola. I programmatori lo fanno per gestire e organizzare grandi quantità di informazioni in modo semplice e versatile.

## Come Fare:

Per lavorare con CSV su Arduino, è necessario utilizzare una libreria specifica come la [ArduinoCSV](https://github.com/rodan/ArduinoCSV). Una volta importata la libreria, è possibile leggere e scrivere dati da e verso un file CSV.

Per esempio, per creare un nuovo file CSV:

```Arduino
#include <ArduinoCSV.h>
CSV file = CSV(F("nuovo_file.csv"));
```

Per aggiungere una riga di dati al file:

```Arduino
file.println("Dato1, Dato2, Dato3");
```

Per leggere una riga dal file:

```Arduino
String line = file.readLine();
```

## Approfondimento:

Il formato CSV è stato sviluppato negli anni '70 quando la prima versione di Microsoft Excel è stata distribuita. Oggi è uno dei formati più utilizzati per salvare dati tabellari. Una alternativa per lavorare con dati tabellari su Arduino è utilizzare un modulo SD per memorizzare e leggere un file CSV dal e verso la sua memoria.

## Vedi Anche:

- [Pagina Wikipedia sul formato CSV](https://it.wikipedia.org/wiki/Comma-separated_values)