---
title:                "Bash: HTML-Analyse"
simple_title:         "HTML-Analyse"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Das Parsen von HTML ist eine wichtige Fähigkeit für jeden, der sich mit Webentwicklung beschäftigt. Es ermöglicht uns, die strukturierten Daten aus einer Webseite zu extrahieren und sie weiterzuverwenden. So können wir beispielsweise Informationen von einer Webseite automatisiert abrufen und in unserer eigenen Anwendung verwenden.

## Wie geht das?

Um HTML zu parsen, müssen wir auf Werkzeuge aus der Bash-Umgebung zurückgreifen. Zu diesen Werkzeugen gehört unter anderem das Programm `sed`, das uns hilft, Textmanipulationen durchzuführen.

Angenommen, wir möchten den Titel einer Webseite auslesen und in einer Variablen speichern. Dazu können wir mit `curl` die Webseite herunterladen und dann `sed` verwenden, um den HTML-Code nach dem Titel-Tag zu durchsuchen und den Text zwischen den `<title>` und `</title>` Tags auszugeben:

```Bash
webseite=$(curl https://www.beispielwebseite.de)
titel=$(echo "$webseite" | sed -n 's/.*<title>\(.*\)<\/title>.*/\1/ p')
echo $titel
```

Die Ausgabe wird der Titel der Webseite sein. Wir können auch andere HTML-Elemente auf diese Weise auslesen, indem wir die Regulären Ausdrücke in `sed` entsprechend anpassen. Weitere Möglichkeiten bieten auch Werkzeuge wie `grep` oder `awk`, die zusätzliche Funktionen für die Textmanipulation bereitstellen.

## Tiefer Einblick

HTML-Parsing kann komplexer werden, wenn wir mit verschachtelten HTML-Strukturen oder CSS-Styling-Elementen arbeiten. Hier müssen wir möglicherweise auf weitere Werkzeuge oder Programmiersprachen wie Python oder JavaScript zurückgreifen. Es gibt auch spezielle Bibliotheken wie `BeautifulSoup` für Python, die das Parsen von HTML vereinfachen können.

Es ist wichtig zu beachten, dass beim Parsen von HTML immer die Struktur der Webseite berücksichtigt werden muss und mögliche Änderungen an der Webseite die Funktionalität unseres Codes beeinflussen können. Es ist daher ratsam, regelmäßig zu überprüfen, ob der Code immer noch wie gewünscht funktioniert.

## Siehe auch

Hier sind einige weitere Ressourcen zum Thema HTML-Parsing:

- Einfache HTML-Parsing Beispiele mit Bash: [https://ostechnix.com/parsing-html-using-sed-awk/](https://ostechnix.com/parsing-html-using-sed-awk/)
- Eine Einführung in die Verwendung von `BeautifulSoup` für Python: [https://www.freecodecamp.org/news/html-parsing-python-beautiful-soup-example-tutorial/](https://www.freecodecamp.org/news/html-parsing-python-beautiful-soup-example-tutorial/)
- Offizielle Dokumentation für `sed`: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)

Happy Coding!