---
title:                "C: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Warum

Das Herunterladen von Webseiten ist ein grundlegender Vorgang in der Welt des Programmierens. Es ermöglicht es uns, Daten aus dem Internet abzurufen und weiterzuverarbeiten. Dies kann für verschiedene Anwendungen nützlich sein, wie z.B. Web-Crawling oder die Erstellung von Datenbanken.

# Wie man eine Webseite herunterlädt

Um eine Webseite herunterzuladen, müssen wir zuerst eine URL angeben, von der wir die Daten erhalten möchten. Wir verwenden die Funktion `curl` aus der Standardbibliothek von C, um die Verbindung zur Webseite herzustellen und die Daten abzurufen. Ein Beispielcode kann wie folgt aussehen:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  // URL der Webseite
  CURL *curl;
  CURLcode res;

  // Verbindung herstellen
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    // Daten speichern
    res = curl_easy_perform(curl);

    // Erfolg oder Fehler ausgeben
    if(res == CURLE_OK)
      printf("Webseite erfolgreich heruntergeladen!");
    else
      printf("Fehler beim Herunterladen der Webseite: %s\n",
             curl_easy_strerror(res));

    // Verbindung schließen
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

# Tiefer Einblick

Beim Herunterladen einer Webseite gibt es mehrere Faktoren zu beachten. Zum Beispiel kann es sein, dass die Webseite weitergeleitet wird, bevor die Daten tatsächlich abgerufen werden. Hier kommt die Option `CURLOPT_FOLLOWLOCATION` ins Spiel, die eingestellt wird, um automatisch den Weiterleitungen zu folgen und die Daten von der endgültigen URL abzurufen.

Außerdem können wir mit der Funktion `curl_easy_setopt` verschiedene Optionen wie User-Agent, Cookies oder Proxy-Einstellungen festlegen, um die Verbindung genauer zu steuern.

In der Standardbibliothek von C gibt es auch andere Funktionen wie `fgets` und `fopen`, mit denen wir Daten von einer Webseite abrufen und speichern können. Jeder Ansatz hat seine Vor- und Nachteile, daher ist es wichtig, die verschiedenen Möglichkeiten zu untersuchen und die Methode zu wählen, die am besten zu unserem Projekt passt.

# Siehe auch

- Offizielle Dokumentation zu `curl`: https://curl.se/libcurl/
- Eine einfache Einführung in das Herunterladen von Webseiten mit C: https://code.tutsplus.com/tutorials/networking-and-socket-programming-tutorial-in-c--cms-25643
- Eine Erklärung der verschiedenen Möglichkeiten, Daten von Webseiten mit C abzurufen: https://computing.tanenbaumbooks.com/Downloads/68_Curtis.pdf