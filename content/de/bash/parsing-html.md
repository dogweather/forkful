---
title:                "HTML analysieren"
html_title:           "Bash: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Sich mit der Analyse von HTML auseinanderzusetzen, kann für viele Gründe relevant sein. Hier sind einige Beispiele:

- Du möchtest automatisiert Daten von einer Webseite extrahieren.
- Du möchtest eine eigene Webseite erstellen und die Grundlage dafür verstehen.

## Wie geht man vor?

Um HTML mit Bash zu parsen, gibt es verschiedene Möglichkeiten. Eine davon ist mithilfe von Regular Expressions (RegEx). Hier ein Beispiel, wie man den Titel einer Webseite auslesen kann:

```Bash
# speichere die Webseiteninhalte in einer Variable
content=$(curl -s https://www.example.com)

# suche nach dem Titel-Tag und extrahiere den Inhalt
title=$(echo "$content" | grep -o '<title>.*</title>' | sed -e 's/<title>//g' -e 's/<\/title>//g')

# gib den Titel aus
echo "Der Titel dieser Webseite lautet: $title"
```

Die Ausgabe sieht dann etwa so aus:

```Bash
Der Titel dieser Webseite lautet: Beispiel Webseite
```

## Tiefentauchen

Um sich tiefer mit der Analyse von HTML auseinanderzusetzen, empfiehlt es sich, die Grundlagen von HTML zu verstehen. HTML steht für Hypertext Markup Language und ist eine Auszeichnungssprache, die für die Strukturierung von Webinhalten verwendet wird.

Um eine Webseite vollständig zu analysieren, kann es sinnvoll sein, sich mit den verschiedenen Elementen von HTML vertraut zu machen und z.B. auch CSS- und JavaScript-Dateien einzubeziehen.

## Siehe auch

- [Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
- [HTML Grundlagen](https://www.w3schools.com/html/default.asp)
- [RegEx Tutorial](https://www.regular-expressions.info/tutorial.html)