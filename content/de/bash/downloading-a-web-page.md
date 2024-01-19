---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Bash-Programmierung: Eine Webseite herunterladen

## Was & Warum?
Herunterladen einer Webseite ist der Prozess, durch den man den Inhalt einer Webseite auf seinen eigenen Computer speichert. Das wird von Programmierern gemacht, um Web-Inhalte offline speichern und analysieren zu können.

## So geht's:
Im Folgenden siehst du die Simplizität des Download-Prozesses einer Webseite mit Bash:

```Bash
#!/bin/bash
# Webseite herunterladen
curl -O https://example.com
```
Nach dem Ausführen erhältst du eine Ausgabe ähnlich wie:

```Bash
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 1270k  100 1270k    0     0  2544k      0 --:--:-- --:--:-- --:--:-- 2544k
```
Dieser Befehl lädt die Webseite example.com herunter und speichert sie auf deinem lokalen Computer.

## Deep Dive

1. Historischer Kontext: Die Fähigkeit zum Herunterladen von Webseiten existiert seit den frühen Tagen des Internets (Mitte der 90er Jahre), als der Datenverkehr noch begrenzt war und Benutzer Webseiten für die schnelle offline Nutzung herunterladen mussten.

2. Alternativen: Es gibt mehrere Methoden zum Herunterladen von Webseiten, darunter Tools wie `wget` und Programmiersprachen wie Python.

3. Implementierungsdetails: `curl` sendet eine HTTP-GET-Anforderung an die angegebene URL. Das Skript speichert dann die Antwort (das ist die Webseite) in eine Datei auf dem lokalen System.

## Siehe auch:

- Mehr Informationen zur Verwendung von `curl`: [Curl Dokumentation](https://curl.haxx.se/docs/manpage.html)
- Alternative Tools: [Wget](https://www.gnu.org/software/wget/)
- Python für das Herunterladen von Webseiten: [Requests: HTTP für Menschen](https://requests.readthedocs.io/de/latest/)