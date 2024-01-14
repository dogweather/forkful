---
title:                "Bash: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Warum

Das Herunterladen von Webseiten gehört zu den grundlegendsten Aspekten der Computerprogrammierung und ist besonders nützlich für Entwickler, die Inhalte von bestimmten Websites extrahieren oder automatisierte Aufgaben ausführen möchten.

# Wie geht das?

Das Herunterladen einer Webseite kann in Bash ganz einfach mit dem Befehl "wget" durchgeführt werden. Hier ist ein Beispiel, um die Webseite von "beispielwebsite.com" herunterzuladen und in eine Datei zu speichern:

```Bash
wget https://www.beispielwebsite.com -O beispiel.html
```
Dieser Befehl lädt die Hauptseite der Website herunter und speichert sie als "beispiel.html" in Ihrem aktuellen Verzeichnis. Sie können auch spezifische Seiten innerhalb einer Website herunterladen, indem Sie die URL für die gewünschte Seite angeben, z.B.:

```Bash
wget https://www.beispielwebsite.com/blog -O blog.html
```
In diesem Beispiel wird die Seite "blog" heruntergeladen und als "blog.html" gespeichert.

## Tiefer gehen

Sie können auch weitere Optionen für den Befehl "wget" nutzen, z.B. um die Dateien in einem bestimmten Verzeichnis oder mit anderen Namenskonventionen zu speichern. Für eine vollständige Liste der verfügbaren Optionen können Sie die man-Seite mit folgendem Befehl aufrufen:

```Bash
man wget
```

Eine andere Möglichkeit, Webseiten herunterzuladen, ist die Verwendung von "curl", einem weiteren nützlichen Befehlszeilenprogramm. Hier ist ein Beispiel, um die Webseite von "beispielwebsite.com" herunterzuladen und in eine Datei zu speichern:

```Bash
curl -o beispiel.html https://www.beispielwebsite.com
```

Die heruntergeladene Datei wird in diesem Fall den gleichen Namen wie die ursprüngliche Webseite haben, aber Sie können dies mit der Option "-o" ändern und einen benutzerdefinierten Namen angeben.

## Siehe auch

- Verwendung von wget (https://www.computerhope.com/unix/wget.htm)
- Verwendung von curl (https://www.computerhope.com/unix/curl.htm)
- Weitere Informationen über das Herunterladen von Webseiten (https://www.lifewire.com/download-entire-website-command-line-interface-2197269)