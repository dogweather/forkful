---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess, bei dem der Inhalt einer bestimmten URL (z.B. HTML, Bilder, CSS) lokal gespeichert wird. Programmierer machen das oft, um Daten zu analysieren, zu manipulieren oder ohne Internetverbindung darauf zugreifen zu können.

## Anleitung:

Der einfachste Weg, eine Webseite in C++ herunterzuladen, geht über die Bibliothek `libcurl`. Hier ist ein einfaches Beispiel:

```C++
#include <curl/curl.h>
#include <fstream>

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();

    std::ofstream out("output.html");
    out << readBuffer;
    out.close();

    return 0;
}
```

Dieser Code lädt eine Webseite herunter und speichert sie in einer Datei namens "output.html".

## Vertiefung:

Früher war es umständlicher, eine Webseite mit C++ herunterzuladen. Die meisten C++-Programmierer benutzten Wininet (unter Windows) oder Sockets (unter Unix). Heutzutage bieten Bibliotheken wie libcurl oder Boost.Asio leistungsfähigere und einfachere Alternativen.

Das Herunterladen einer Webseite involviert in der Regel den Aufbau einer TCP-Verbindung zu einem Server an der spezifizierten URL, das Senden einer HTTP-Anforderung und das Empfangen der Serverantwort als HTML- oder Binärdaten.

## Siehe Auch:

Um mehr über das Web-Scraping und das Herunterladen von Webseiten mit C++ zu lernen, besuchen Sie bitte diese Ressourcen:

1. [Libcurl-Dokumentation](https://curl.se/libcurl/c/)
2. [Boost.Asio-Dokumentation](https://www.boost.org/doc/libs/1_73_0/doc/html/boost_asio.html)
3. [HTTP Made Really Easy - A Practical Guide to Writing Clients and Servers](http://www.jmarshall.com/easy/http/)
4. [C++ Netzwerkprogrammierung mit Boost.Asio](https://dieboostcppbibliotheken.de/boost.asio)