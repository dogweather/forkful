---
title:                "Gleam: Löschen von Zeichen, die einem bestimmten Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem bestimmten Muster entsprechen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Oftmals müssen Programmiererinnen und Programmierer Zeichen aus einem String löschen, die einer bestimmten Vorlage entsprechen. Dies kann aus verschiedenen Gründen erforderlich sein, wie zum Beispiel das Entfernen von Leerzeichen, Sonderzeichen oder Formatierungen. Mit Gleam ist dies einfach und schnell zu erledigen, daher zeigen wir in diesem Blogpost, wie es geht.

## Wie man Zeichen einer bestimmten Vorlage löscht

Das Löschen von Zeichen einer bestimmten Vorlage in Gleam ist sehr einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir jedoch das Modul `String` importieren, um auf die nötigen Funktionen zugreifen zu können.

```Gleam
import gleam/string

chars_to_delete = ".,!? "
original_string = "Hello, world!"
output = string.remove(original_string, |c| c in chars_to_delete)

gleam => "Hello world"
```

In diesem Beispiel verwenden wir die Funktion `string.remove`, die zwei Parameter erwartet: den ursprünglichen String und eine Funktion, die bestimmt, welche Zeichen gelöscht werden sollen. In unserem Fall übergeben wir eine Funktion, die prüft, ob das aktuelle Zeichen in `chars_to_delete` enthalten ist. Das Ergebnis ist ein neuer String ohne die gelöschten Zeichen.

## Tiefergehende Informationen

Es gibt verschiedene Möglichkeiten, um Zeichen in Gleam zu löschen. Eine davon ist die Verwendung der Funktion `string.filter`, die ähnlich wie `string.remove` funktioniert, jedoch ein Ergebnis zurückgibt, das nur die übrig gebliebenen Zeichen enthält. Diese Funktion ist besonders nützlich, wenn man bestimmte Zeichen ersetzen oder durch andere ersetzen möchte.

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken, die in Gleam mithilfe des Moduls `Regex` verwendet werden können. Mit regulären Ausdrücken können komplexere Muster definiert werden, nach denen Zeichen gelöscht werden können.

## Siehe auch

- Offizielle Gleam Dokumentation zu `string` Modul: https://gleam.run/docs/stdlib/string/
- Einführung in reguläre Ausdrücke in Gleam: https://dev.to/gbili/gleam-and-regular-expressions-1h9p