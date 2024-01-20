---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, ihre Daten über das Internet zu beziehen und lokal zu speichern. Programmierer tun dies oft, um Informationen für die Datenanalyse zu sammeln oder um offline auf Inhalt zuzugreifen.

## So geht's:

C bietet mehrere Bibliotheken zum Herunterladen von Webseiten, aber wir konzentrieren uns hier auf die `libcurl` Bibliothek. Hier ist ein einfacher Code, der eine Webseite herunterlädt und ihre Daten auf der Konsole ausgibt.

```C
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

int main(void)
{
  CURL *curl_handle;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_ALL);
  curl_handle = curl_easy_init();

  curl_easy_setopt(curl_handle, CURLOPT_URL, "http://example.com");
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);

  res = curl_easy_perform(curl_handle);

  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

  curl_easy_cleanup(curl_handle);
  curl_global_cleanup();

  return 0;
}
```
Dieser Code würde die gesamte HTML-Ausgabe von `http://example.com` auf die Konsole ausgeben.

## Tiefere Einblicke

Webseiten herunterzuladen ist ein grundlegender Aspekt des Web-Scrapings und wurde seit den frühen Tagen des Internets praktiziert. Es gibt viele andere Bibliotheken und Techniken, um dies in verschiedenen Programmiersprachen zu erreichen, einschließlich Python's `requests` und JavaScript's `axios`.

Die `libcurl` Bibliothek, die wir in diesem Artikel verwenden, ist eine leistungsstarke und flexible Möglichkeit, dies in C zu tun. Sie ermöglicht es uns, HTTP-GET-Anfragen zu senden und die Antwort zu empfangen. Dabei werden Callback-Funktionen wie `write_data` in unserem Beispiel genutzt, um die empfangenen Daten zu verarbeiten und in diesem Fall auf der Konsole auszugeben.

## Siehe auch

Für weitere Informationen, bitte sehen Sie die folgenden Ressourcen:

- [`libcurl` offizielle Dokumentation](https://curl.haxx.se/libcurl/c/)
- [Das `libcurl-tutorial`](https://curl.se/libcurl/c/libcurl-tutorial.html)
- [Andere Methoden zum herunterladen von Webseiten in C](http://zetcode.com/articles/curl/)