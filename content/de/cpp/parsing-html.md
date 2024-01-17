---
title:                "HTML-Auswertung"
html_title:           "C++: HTML-Auswertung"
simple_title:         "HTML-Auswertung"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-html.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Parsen von HTML ist der Prozess des Analysierens von HTML-Code, um die Struktur und Syntax der darin enthaltenen Informationen zu verstehen. Programmierer tun dies, um Webseiten aufzubereiten, Daten zu extrahieren oder um automatisierte Aufgaben wie Web Scraping durchzuführen.

# Wie geht's:
Das Parsen von HTML kann in C++ mit Hilfe vieler Bibliotheken durchgeführt werden, wie z.B. RapidXML oder LibXML. Ein kurzes Beispiel, um ein HTML-Dokument zu parsen und die Title-Tags zu extrahieren:

```C++
#include <iostream>
#include <libxml/HTMLparser.h>

int main() {
    // HTML-Dokument lesen und analysieren
    const char* htmlString = "<html><head><title>Beispiel Webseite</title></head><body><h1>Willkommen</h1></body></html>";
    htmlDocPtr doc = htmlReadDoc((xmlChar*)htmlString, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    // Title-Tags extrahieren
    xmlNode* titleNode = xmlDocGetRootElement(doc)->children->children->next->children;
    xmlChar* title = xmlNodeListGetString(doc, titleNode, 1);
    
    // Output
    std::cout << "Titel: " << title << std::endl;
    
    // Speicher freigeben
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 0;
}
```
Output: Titel: Beispiel Webseite

# Tief eintauchen:
Das Parsen von HTML hat eine lange Geschichte, da HTML selbst immer weiterentwickelt wurde. Heutzutage gibt es viele Alternativen zum Parsen von HTML, wie z.B. BeautifulSoup für Python oder Jsoup für Java. In C++ kann auch auf reguläre Ausdrücke zurückgegriffen werden, um HTML zu parsen, aber dies ist oft komplexer und kann zu Fehlern führen.

Bei der Implementierung des Parsens von HTML in C++ ist zu beachten, dass HTML sehr unvorhersehbar ist und möglicherweise nicht immer den Standards entspricht. Zusätzlich können Sonderzeichen und verschachtelte Tags die Analyse erschweren.

# Siehe auch:
- https://www.w3.org/html/
- https://github.com/dutitliput/chtml-parser