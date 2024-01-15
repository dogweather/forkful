---
title:                "Herunterladen einer Webseite"
html_title:           "C: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Du möchtest eine Webseite herunterladen? Das kann viele Gründe haben. Vielleicht möchtest du sie offline lesen, sie in einem Projekt verwenden oder einfach nur speichern, um später darauf zurückzugreifen. Egal aus welchem Grund, es ist gar nicht so schwer, eine Webseite herunterzuladen, wie du vielleicht denkst.

## Wie geht das?

Um eine Webseite mit C herunterzuladen, brauchst du die richtigen Werkzeuge und ein bisschen Code. Zuerst musst du eine Bibliothek wie cURL herunterladen, die es dir ermöglicht, auf Webseiten zuzugreifen. Dann kannst du mit ein paar einfachen Befehlen eine Seite herunterladen und den HTML-Code anzeigen lassen.

```C
// Beispieldatei laden
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  FILE *fp;
  // Datei erstellen, in die die Webseite gespeichert wird
  fp = fopen("beispiel.html", "w");
  // Webseite von URL herunterladen
  curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
  // Webseiteninhalt in Datei schreiben
  curl_easy_perform(curl);
  // Los geht's!
  fclose(fp);
  // Inhalt der heruntergeladenen Datei anzeigen
  system("cat beispiel.html");
  return 0;
}
```

Das Ergebnis wird so aussehen:

```
<!doctype html>
<html>

<head>
  <title>Beispiel Domain</title>

  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
    body {
      font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      overflow-x: hidden;
      margin: 0;
      padding: 0;
    }

    body>div {
      ...

```

## Tieferer Einblick

Um eine Webseiten zu verstehen, musst du wissen, wie sie aufgebaut ist. Der HTML-Code, den wir erhalten haben, enthält alle Informationen, die für die Anzeige der Webseite im Browser benötigt werden. Wenn du mehr über HTML, CSS oder JavaScript lernen möchtest, gibt es viele Ressourcen online, die dir dabei helfen können.

## Siehe auch

- https://curl.haxx.se/
- https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
- https://www.w3schools.com/html/default.asp