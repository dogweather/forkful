---
title:                "C++: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen einer Webseite mithilfe von C++ kann sehr nützlich sein, wenn man Informationen von einer bestimmten Seite extrahieren oder analysieren möchte. Dies kann besonders hilfreich sein, wenn man eine große Menge von Daten sammeln muss.

## Wie man es macht

Um eine Webseite mit C++ herunterzuladen, müssen wir die entsprechenden Bibliotheken importieren und eine Verbindung zu der URL herstellen, von der wir die Daten holen möchten. Dann müssen wir die Daten in ein Datenobjekt speichern und können sie anschließend nach unseren Bedürfnissen verarbeiten.

```C++
#include <iostream>
#include <curl/curl.h> // Importieren der CURL-Bibliothek

int main()
{
  CURL *curl; // Initialisieren eines CURL-Objekts
  CURLcode res; // Statuscode für die Verbindung
  
  // URL, von der wir die Daten herunterladen möchten
  std::string url = "https://www.beispiel.de";
  
  // Verbindung herstellen
  curl = curl_easy_init();
  if(curl) 
  {
    // URL setzen
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    
    // User-Agent setzen (optional)
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "curl/7.47.0");
    
    // Daten speichern
    res = curl_easy_perform(curl);
    
    // Verbindung schließen
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Die obige Beispielcodes zeigen, wie man eine Verbindung zu einer URL herstellt und die Daten in einem CURL-Objekt speichert. Durch Hinzufügen weiterer Optionen zu `curl_easy_setopt` können wir beispielsweise bestimmen, in welchem Format die Daten gespeichert werden sollen oder ob wir zusätzliche Informationen wie Header abrufen möchten.

## Tiefere Einblicke

Um genauer zu verstehen, wie das Herunterladen einer Webseite funktioniert, können wir uns die Funktion `curl_easy_perform` genauer ansehen. Diese Funktion ist für das Ausführen der durch die vorherigen Optionen festgelegten Aktionen verantwortlich. Sie gibt den Statuscode der Verbindung zurück, was uns hilft, zu überprüfen, ob das Herunterladen erfolgreich war oder nicht.

Darüber hinaus gibt es noch weitere Möglichkeiten, eine Verbindung mit einer URL herzustellen, zum Beispiel mithilfe der Bibliotheken `libcurl` oder `libwww`. Auch diese bieten verschiedene Optionen und Funktionen für das Herunterladen von Webseiten.

## Siehe auch

- [Curl documentation](https://curl.haxx.se/libcurl/)
- [Libcurl Code Example](https://curl.haxx.se/lxr/source/docs/examples/https.c)
- [Libwww documentation](https://www.w3.org/Library/)
- [Libwww Code Example](https://wiki.w3.org/Library/Cookbook/HTTP_Web_Get)