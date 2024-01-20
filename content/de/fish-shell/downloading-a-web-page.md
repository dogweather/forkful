---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess, in dem der gesamte Inhalt einer bestimmten Webseite auf den lokalen Computer übertragen wird. Programmierer tun dies oft, um den Inhalt einer Webseite für Datenanalyse, Web-Scraping oder Offline-Nutzung zu sichern.

## So geht's:

```Fish Shell
# Installiere httpie mit
brew install httpie

# Lade eine Webseite herunter
http -d example.com > page.html
```

Die obigen Anweisungen installieren zuerst `httpie`, ein Befehlswerkzeug, das zum Senden von HTTP-Anfragen verwendet wird. Danach lädt das `http`-Kommando die Webseite `example.com` herunter und speichert sie als `page.html`.

## Vertiefung:

Die Praxis des Herunterladens von Webseiten reicht bis in die frühen Tage des Internets zurück, als die Verbindungsgeschwindigkeiten oft so langsam waren, dass das Offline-Lesen von Webseiten eine wünschenswerte Option war. Heutzutage ist das Herunterladen von Webseiten für Datenwissenschaftler, Web-Crawler und Archivierer immer noch wichtig.

Alternativen zu `httpie` sind unter anderem `wget` und `curl`. Während `httpie` für seine Benutzerfreundlichkeit geschätzt wird, bieten `wget` und `curl` mehr Anpassungsmöglichkeiten und werden häufiger in bestimmten Programmierumgebungen verwendet.

Die Implementation des Herunterladens einer Webseite mit `httpie` in der Fish Shell ist ziemlich unkompliziert. Die Anfrage wird an die Ziel-URL gesendet und der Antwortbody (die Webseite) wird in eine Datei auf dem lokalen System gespeichert.

## Siehe Auch:

- HTTPie Dokumentation: https://httpie.org/doc 
- Wget Anleitung: https://www.gnu.org/software/wget/ 
- Curl Anleitung: https://curl.haxx.se/docs/manpage.html