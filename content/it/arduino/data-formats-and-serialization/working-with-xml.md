---
title:                "Lavorare con XML"
date:                  2024-01-26T04:27:30.963428-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Lavorare con XML su Arduino implica l'analisi e la manipolazione dei dati XML, che di solito provengono da API web o file di configurazione. I programmatori lo fanno per integrarsi con servizi che utilizzano XML per lo scambio di dati o per memorizzare dati in un formato strutturato e leggibile dall'uomo.

## Come fare:
Useremo la libreria `XMLWriter` per creare XML e la libreria `tinyxml2` per analizzarlo. Installa prima le librerie tramite il Gestore Librerie nel tuo IDE Arduino.

Creare un documento XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Utilizzando Serial per l'output
  
  xml.header();
  xml.tag("greeting").tag("text").testo("Ciao, mondo!").close().close();
  xml.flush();
}

void loop() {
}
```

Decodifica di una stringa XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Ciao, mondo!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Esempio di output:

```
<greeting>
  <text>Ciao, mondo!</text>
</greeting>
```

## Approfondimento
XML, o Extensible Markup Language, è un linguaggio di markup che definisce un insieme di regole per la codifica dei documenti in un formato che è sia leggibile dall'uomo che dalla macchina. Esiste dalla fine degli anni '90 ed è ampiamente utilizzato in vari campi, specialmente dove è necessario lo scambio di dati indipendente dalla piattaforma. Le limitate risorse di memoria di Arduino rendono il lavoro con XML più sfidante rispetto a un PC. Pertanto, le librerie leggere sono cruciali. Anche se JSON ha guadagnato popolarità per lo scambio di dati a causa della sua sintassi più semplice e della minore impronta, XML è ancora ampiamente utilizzato, specialmente quando si ha a che fare con sistemi legacy o applicazioni che richiedono la validazione dei documenti tramite schemi. La chiave per l'implementazione di XML su Arduino è l'analisi in streaming, che legge il documento in segmenti per mantenere basso l'utilizzo della memoria.

## Vedi Anche
- [Documentazione della Libreria TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Libreria Arduino JSON](https://arduinojson.org/) come alternativa per lavorare con i dati JSON.
- [Tutorial XML di W3Schools](https://www.w3schools.com/xml/) per l'apprendimento generale di XML.
- [Specifiche XML del W3C](https://www.w3.org/XML/) per gli standard ufficiali XML e raccomandazioni.
