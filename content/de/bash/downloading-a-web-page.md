---
title:                "Eine Webseite herunterladen"
html_title:           "Bash: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Jeder, der an der Webentwicklung oder dem Website-Management interessiert ist, muss in der Lage sein, Webseiten herunterzuladen. Dies ermöglicht es ihnen, Änderungen vorzunehmen, zu überprüfen, ob ihre Inhalte korrekt angezeigt werden und die Leistung ihrer Website zu analysieren.

## Wie geht das?

Das Herunterladen einer Webseite in Bash ist eine einfache und effektive Methode. Hier sind einige Beispiele, wie du das mit deinem Terminal machen kannst:

```Bash
# Gib die Webseite aus
curl https://www.meinewebsite.de

# Speichere die Webseite als HTML-Datei
curl https://www.meinewebsite.de -o index.html

# Liste alle Links auf der Seite auf
curl https://www.meinewebsite.de | grep -o 'href="[^\"]\+"' | cut -d'"' -f2
```

Die Verwendung von `curl` ermöglicht es dir, alle Arten von Optionen und Befehlen anzupassen, um deine spezifischen Bedürfnisse zu erfüllen. Du kannst sogar HTTP-Authentifizierung verwenden, indem du Benutzername und Passwort im Befehl einschließt.

## Tiefer tauchen

Wenn du tiefer in das Thema eintauchen möchtest, gibt es einige nützliche Dinge, über die du mehr erfahren kannst. Zum Beispiel:

- Die Verwendung von `wget` anstelle von `curl` bietet mehr Möglichkeiten und Funktionalitäten beim Herunterladen von Webseiten.
- Du kannst einen benutzerdefinierten Header oder eine Referenz-URL hinzufügen, um die Webseite abzurufen.
- Verwende `wget` mit dem Parameter `-m` für eine vollständige Spiegelung einer Website.

## Siehe auch

- [Offizielle Dokumentation für `curl`](https://curl.se/docs/manpage.html)
- [Offizielle Dokumentation für `wget`](https://www.gnu.org/software/wget/manual/wget.html)
- [Weiterführende Informationen zum Herunterladen von Webseiten mit Bash](https://osric.com/chris/accidental-developer/2018/05/downloading-a-website-with-curl-wget/)

Danke, dass du diesen Artikel gelesen hast! Hoffentlich hast du nun ein besseres Verständnis dafür, wie du Webseiten mit Bash herunterladen kannst. Viel Spaß beim Ausprobieren und Entdecken!