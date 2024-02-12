---
title:                "Lavorare con XML"
aliases:
- /it/cpp/working-with-xml/
date:                  2024-01-26T04:28:31.058357-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con XML significa analizzare, creare e manipolare dati XML (eXtensible Markup Language). I programmatori gestiscono XML per gestire lo scambio di dati strutturati, la configurazione e altro ancora, grazie alla sua natura neutrale rispetto alla piattaforma.

## Come fare:
Ecco un modo semplice per analizzare XML utilizzando la libreria TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Ciao, Mondo!</message></root>");
    const char* contenuto = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << contenuto << std::endl;
    return 0;
}
```

Output di esempio:

```
Ciao, Mondo!
```

E questo è come si crea un file XML:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* dichiarazione = doc.NewDeclaration();
    doc.InsertFirstChild(dichiarazione);
    auto* radice = doc.NewElement("root");
    doc.InsertEndChild(radice);
    auto* messaggio = doc.NewElement("message");
    messaggio->SetText("Ciao, Mondo!");
    radice->InsertEndChild(messaggio);
    doc.SaveFile("output.xml");
    return 0;
}
```

Questo genera un file XML `output.xml` con contenuti:

```xml
<?xml version="1.0"?>
<root>
    <message>Ciao, Mondo!</message>
</root>
```

## Approfondimento
XML è stato fondamentale nei servizi web e nello stoccaggio dati dalla fine degli anni '90. Sebbene JSON e YAML siano ora più comuni per la configurazione e l'interop, XML è ancora molto diffuso in molti sistemi aziendali. Analizzare XML in C++ può sembrare antiquato con l'analisi manuale DOM/SAX. Fortunatamente, biblioteche come TinyXML-2 lo semplificano. C++ non ha supporto integrato per XML; biblioteche come TinyXML-2, pugixml o Xerces semplificano le parti più complicate.

## Vedi Anche
- Documentazione TinyXML-2: https://leethomason.github.io/tinyxml2/
- Biblioteca pugixml: https://pugixml.org/
- Parser Xerces-C++: https://xerces.apache.org/xerces-c/
- Specifica XML W3C: https://www.w3.org/XML/
