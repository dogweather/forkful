---
title:    "Ruby: Großschreibung eines Strings"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitälisieren von Zeichenfolgen ist eine gängige Aufgabe in der Programmierung. Es kann hilfreich sein, um die Nutzerfreundlichkeit zu verbessern, bestimmte Datenformatierungen zu erstellen oder einfach nur um den Code lesbarer zu gestalten.

## Wie geht man vor

Um eine Zeichenfolge in Ruby zu kapitalisieren, kann man den Befehl `.capitalize` nutzen. Hier ein Beispiel:

```Ruby
name = "max mustermann"
puts name.capitalize
```

Dieser Code wird die Zeichenfolge "max mustermann" in "Max musterman" ändern.

Zusätzlich gibt es die Möglichkeit, die Methode `.upcase` zu nutzen, um alle Buchstaben in Großbuchstaben zu verwandeln, oder `.downcase`, um sie in Kleinbuchstaben zu ändern. Hier ist ein Beispiel:

```Ruby
name = "Max Mustermann"
puts name.upcase
puts name.downcase
```

Die Ausgabe wird "MAX MUSTERMANN" und "max mustermann" sein.

## Tiefergehende Informationen

In Ruby gibt es verschiedene Möglichkeiten, eine Zeichenfolge zu kapitalisieren. Die `.capitalize`-Methode ist eine einfache Möglichkeit, um die erste Buchstabe eines Satzes zu einem Großbuchstaben zu ändern. Wenn jedoch eine Zeichenfolge mehr als einen Satz enthält, wird nur der erste Buchstabe jedes Satzes geändert.

Um alle Wörter in einer Zeichenfolge zu kapitalisieren, kann die Methode `.titleize` verwendet werden. Diese Methode wird auch bestimmte Wörter wie "a" oder "the" ausschließen, es sei denn, sie sind der erste Teil des Satzes.

Außerdem gibt es die Möglichkeit, benutzerdefinierte Regeln für die Groß- und Kleinschreibung zu erstellen, indem man das `I18n`-Modul verwendet.

## Siehe auch

Hier sind einige nützliche Links für weitere Informationen zum Thema Zeichenfolgenkapitalisierung in Ruby:

[Offizielle Ruby-Dokumentation zur String-Klasse](https://ruby-doc.org/core-2.7.1/String.html)

[Blog-Beitrag über verschiedene Methoden zur Zeichenfolgenkapitalisierung in Ruby](https://dzone.com/articles/14-ways-to-manipulate-a-ruby-string)

[Tutorial zur Benutzung des I18n-Moduls in Ruby](https://guides.rubyonrails.org/i18n.html)