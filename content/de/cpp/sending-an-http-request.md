---
title:                "Senden einer http Anfrage"
html_title:           "C++: Senden einer http Anfrage"
simple_title:         "Senden einer http Anfrage"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum
Wer sich mit der Erstellung von Webanwendungen beschäftigt, kommt früher oder später nicht umhin, HTTP Anfragen zu verwenden. Diese ermöglichen es, Daten von einer Webanwendung an eine andere zu übertragen. Es ist also unerlässlich zu verstehen, wie man eine HTTP Anfrage in C++ senden kann.

## Wie das geht
Mithilfe der Bibliothek `libcurl` kann man in C++ ganz einfach HTTP Anfragen versenden. Zunächst müssen wir jedoch die entsprechenden Header-Dateien einbinden, um die Funktionen von `libcurl` nutzen zu können:
```C++
#include <curl/curl.h>
```
Als nächstes müssen wir einen `CURL` Handle erstellen, der es uns ermöglicht, eine Verbindung aufzubauen und die Anfrage zu versenden:
```C++
CURL *handle = curl_easy_init();
```
Nun können wir die URL festlegen, an die die Anfrage gesendet werden soll, und sie dem `CURL` Handle zuweisen:
```C++
const char* url = "http://beispiel.com/meine-anfrage";
curl_easy_setopt(handle, CURLOPT_URL, url);
```
Als nächstes müssen wir die gewünschten Anfrageparameter definieren. Dies kann mithilfe von `curl_easy_setopt` gemacht werden, wobei wir verschiedene Optionen wie beispielsweise die Methode der Anfrage (GET, POST, etc.) oder die Daten, die übertragen werden sollen, festlegen können. Im folgenden Beispiel senden wir eine einfache GET Anfrage, die keine zusätzlichen Parameter benötigt:
```C++
curl_easy_setopt(handle, CURLOPT_HTTPGET, 1L);
```
Schließlich können wir die Anfrage mit `curl_easy_perform` abschicken und die Antwort in einer Variable speichern:
```C++
CURLcode res;
res = curl_easy_perform(handle);
```
Als Ergebnis erhalten wir den Statuscode der Anfrage sowie die empfangenen Daten oder eine eventuelle Fehlermeldung.

## Tiefere Einblicke
Die Bibliothek `libcurl` bietet viele verschiedene Optionen und Konfigurationsmöglichkeiten, um Anfragen individuell anzupassen. Es ist auch möglich, mehrere Anfragen parallel zu versenden oder Anfragen mit komplexeren Parametern zu erstellen. Es lohnt sich daher, sich genauer mit der Dokumentation von `libcurl` auseinanderzusetzen, um das volle Potenzial auszuschöpfen.

## Siehe auch
- [Dokumentation von `libcurl`](https://curl.haxx.se/libcurl/c/)
- [Tutorial zur Verwendung von `libcurl` in C++](https://blog.codecentric.de/2016/06/http-anfragen-cpp-libcurl/)