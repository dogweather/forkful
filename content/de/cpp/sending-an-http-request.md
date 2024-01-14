---
title:                "C++: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum
## Wie

Das Senden von HTTP-Anfragen ist für Programmierer von C++ von entscheidender Bedeutung, wenn sie mit Webanwendungen oder Webdiensten arbeiten wollen. Eine HTTP-Anfrage ermöglicht es einem Programm, mit einem Server zu kommunizieren und Daten von diesem zu erhalten oder zu senden.

Um eine HTTP-Anfrage in C++ zu senden, müssen wir zuerst die notwendigen Bibliotheken und Header-Dateien einbinden. Wir verwenden die `curl` Bibliothek, um HTTP-Anfragen zu senden und die Antwort vom Server zu erhalten.

```C++
#include <iostream>
#include <curl/curl.h>
```

Als nächstes definieren wir eine Funktion, die die notwendigen Parameter für die HTTP-Anfrage enthält. Wir geben die URL an, an die die Anfrage gesendet werden soll, und die Art der Anfrage, die wir senden wollen. In diesem Beispiel senden wir eine einfache GET-Anfrage.

```C++
CURL* curl;
curl = curl_easy_init();
if(curl) {
    CURLcode res;
    // Definieren der URL
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    // Definieren der Anfragemethode
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");

    // Ausführen der Anfrage und Speichern der Antwort
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
        std::cout << "Fehler beim Senden der HTTP-Anfrage: " << curl_easy_strerror(res) << std::endl;
    else
        std::cout << "Anfrage erfolgreich gesendet." << std::endl;

    // Aufräumen
    curl_easy_cleanup(curl);
}
```

Die `curl_easy_setopt()` Funktion ermöglicht es uns, verschiedene Optionen für die Anfrage festzulegen. In diesem Beispiel definieren wir die URL und die Anfragemethode, aber es gibt noch viele weitere Optionen, die wir festlegen können, wie z.B. die Verwendung von Authentifizierung oder das Senden von Daten mit der Anfrage.

## Tiefergehender Einblick

Die `curl` Bibliothek ist ein mächtiges Werkzeug, wenn es um das Senden von HTTP-Anfragen geht. Es ermöglicht uns, verschiedene Arten von Anfragen zu senden, Daten zu senden und zu empfangen, und sogar Authentifizierung zu verwenden.

Es gibt auch andere Bibliotheken und Frameworks, die uns beim Senden von HTTP-Anfragen helfen können, wie z.B. `httplib` oder `Boost.Asio`. Sie alle haben ihre Vor- und Nachteile, aber am Ende ist es wichtig, die Funktionen zu verstehen, die sie bieten, und die beste Option für die jeweilige Anwendung auszuwählen.

In diesem Beispiel haben wir nur eine GET-Anfrage gesendet, aber mit diesen Tools können wir auch POST-, PUT- und DELETE-Anfragen senden, um mit einer Webanwendung oder einem Webdienst zu interagieren.

Also, wenn du jemals in der Situation bist, eine HTTP-Anfrage in deinem C++ Programm zu senden, weißt du jetzt, dass es möglich ist und wie es gemacht wird.

# Siehe auch

- [curl website](https://curl.se/)
- [httplib github repository](https://github.com/yhirose/cpp-httplib)
- [Boost.Asio documentation](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html)