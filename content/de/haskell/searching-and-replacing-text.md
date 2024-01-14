---
title:    "Haskell: Suche und Ersetzen von Text"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Textsuche und -ersetzung sind wichtige Fähigkeiten, die oft in der Programmierung benötigt werden. Mit der Zeit kann der Code unübersichtlich werden und es ist wichtig, die richtigen Werkzeuge zu haben, um diesen Prozess einfacher und effizienter zu gestalten.

# Wie Geht's

Um Text in Haskell zu suchen und zu ersetzen, müssen wir die Funktion `subRegex` aus dem Paket `Text.Regex` verwenden. Wir müssen auch die Optionen `global` und `caseSensitive` festlegen, um sicherzustellen, dass alle Übereinstimmungen in unserem Text gesucht und der Groß- und Kleinschreibung beachtet werden.

Beispiel:
```Haskell 
import Text.Regex

-- Suchen und Ersetzen von "Hallo" durch "Guten Tag"
main = subRegex (mkRegex "Hallo") "Guten Morgen, Hallo!" "Guten Tag" global caseSensitive
```
Ausgabe:
```
"Guten Morgen, Guten Tag!"
```

# Tiefergehende Analyse

Das Paket `Text.Regex` bietet auch Optionen für die Verwendung von regulären Ausdrücken, um die Suche und den Ersatz von Text noch genauer zu gestalten. Wir können zum Beispiel Platzhalter wie `.` verwenden, um alle Zeichen an einer bestimmten Stelle zu repräsentieren, oder `*`, um an beliebiger Stelle im Text zu suchen.

Beispiel:
```Haskell 
import Text.Regex

-- Suchen und Ersetzen von "Wort1" durch "Wort2" an beliebiger Stelle 
main = subRegex (mkRegex ".*Wort1.*") "Dies ist ein Text mit dem Wort1 hier drin." "Wort2" global caseSensitive
```
Ausgabe:
```
"Dies ist ein Text mit dem Wort2 hier drin."
```

# Siehe Auch

- Das offizielle Haskell-Dokumentation zum Paket `Text.Regex`: https://hackage.haskell.org/package/regex

- Weitere Informationen über reguläre Ausdrücke: https://www.regular-expressions.info/