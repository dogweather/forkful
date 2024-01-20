---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage ist ein Prozess, bei dem ein Client eine Nachricht an einen Server sendet, um bestimmte Daten oder Dienste zu erfragen. Programmierer tun dies, um Netzwerkressourcen zu interagieren, sei es zum Abrufen, Senden oder Aktualisieren von Daten.

## So geht's:

In C++ können wir eine HTTP-Anfrage mit der `cpr` Bibliothek senden. Installieren Sie zunächst die `cpr` Bibliothek:

```bash
vcpkg install cpr
```

Ein einfaches Beispiel für eine GET-Anfrage sieht so aus:

```C++
#include <cpr/cpr.h>

int main(int argc, char **argv) {
    cpr::Response res = cpr::Get(cpr::Url{"https://www.google.com"});
    std::cout << res.status_code << std::endl; // 200
    std::cout << res.text << std::endl;        // HTML-Inhalt der Google-Startseite
    return 0;
}
```
Das Programm sendet eine GET-Anfrage an google.com und druckt dann den Statuscode (200) und den Text der Antwort aus.

## Vertiefen

HTTP-Anfragen sind seit Anfang des Internets ein grundlegender Bestandteil des Web. Es gibt viele Möglichkeiten, HTTP-Anfragen in C++ zu senden, `cpr` ist jedoch eine der moderneren und einfacheren Bibliotheken. Einige Alternativen sind `libcurl`, `Boost.Asio` und `Poco`.

Beachten Sie, dass cpr auf libcurl basiert und praktisch ein moderner C++-Wrapper dafür ist. Die eigentliche Anforderung wird mit den von libcurl bereitgestellten Funktionen gesendet. Das heisst, Sie können die gleiche Funktionalität mit libcurl erreichen, obwohl der Code komplexer und schwerer zu verstehen wäre.

## Siehe Auch

- CPR GitHub Repository: https://github.com/whoshuu/cpr
- Die CPR-Dokumentation: https://docs.libcpr.org
- HTTP-Anforderungen mit libcurl senden: https://curl.haxx.se/libcurl/c/http3.html
- HTTP-Anforderungen mit Boost.Asio: https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio/example/cpp11/http/client/sync_client.cpp
- HTTP-Anforderungen mit Poco: https://pocoproject.org/docs/Poco.Net.HTTPClientSession.html