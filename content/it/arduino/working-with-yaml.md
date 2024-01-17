---
title:                "Lavorare con yaml"
html_title:           "Arduino: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

YAML, acronimo di YAML Ain't Markup Language, è un formato di dati leggibile dall'uomo e utilizzato per organizzare e rappresentare informazioni strutturate. I programmatori lo utilizzano perché è semplice da leggere e scrivere, permette di gestire grandi quantità di dati in modo ordinato ed è compatibile con molte piattaforme diverse.

## Come fare:

Per utilizzare YAML in Arduino, è necessario installare una libreria apposita. Segui questi semplici passaggi:

1. Apri l'IDE di Arduino e vai su **Sketch > Includi Libreria > Gestore Librerie**.
2. Digita "YAML" nella barra di ricerca e premi il pulsante *Installa* accanto alla libreria *ArduinoJson*.
3. Per utilizzare la libreria, aggiungi il seguente codice all'inizio del tuo sketch:

```Arduino
#include <ArduinoJson.h>
```

4. Ora sei pronto per lavorare con YAML!

## Approfondimento:

YAML è stato creato nel 2001 per rispondere alla mancanza di un formato di dati semplice da leggere e scrivere, adatto per le piattaforme web. È spesso utilizzato in sostituzione di XML, ma a differenza di quest'ultimo, YAML non è un linguaggio di markup. Oltre ad Arduino, è supportato da molti altri linguaggi di programmazione come Python, Java ed altri.

## Vedi anche:

- [Documentazione ufficiale di YAML](https://yaml.org/)
- [Esempi pratici sulla gestione di dati con YAML](https://www.arduino.cc/en/Tutorial/YamlParsing)
- [Altro esempio di utilizzo di YAML con Arduino](https://forum.arduino.cc/index.php?topic=243360.0)