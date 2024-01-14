---
title:    "Gleam: Text suchen und ersetzen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele verschiedene Gründe, warum jemand gezielt nach Text suchen und ihn ersetzen möchte. Vielleicht möchtest du bestimmte Wörter oder Formulierungen in deinem Code ändern, um ihn übersichtlicher zu gestalten oder an eine bestimmte Sprachkonvention anzupassen. Oder du möchtest einfach nur eine Textdatei durchsehen und einige Begriffe durch andere ersetzen. In jedem Fall kann dies mit dem Gleam-Programmierungsframework schnell und effizient erledigt werden.

## Wie geht das?

Gleam bietet eine Vielzahl von Funktionen zum Suchen und Ersetzen von Text. Eines der einfachsten und am häufigsten verwendeten ist `String.replace`, das ein Muster und einen Ersatztext als Argumente akzeptiert. Eine typische Verwendung wäre:

```Gleam
x = String.replace("Gle", "Ble", "Gleam ist fantastisch!")
```

Die obige Zeile würde "Bleam ist fantastisch!" zurückgeben. Beachte, dass beim Suchen und Ersetzen die Groß- und Kleinschreibung berücksichtigt wird.

Ein weiteres nützliches Feature ist die `Regex.replace` Funktion, die reguläre Ausdrücke unterstützt. Dadurch wird es möglich, komplexere Suchmuster zu definieren und mehrere Ersetzungen in einem Durchlauf durchzuführen. Zum Beispiel könnte man alle Vorkommen von "Gleam" mit "Bleam" ersetzen und dabei die Groß- und Kleinschreibung ignorieren:

```Gleam
x = Regex.replace(~r/Gleam/i, "Bleam", "Gleam ist fantastisch!")
```

Weitere Informationen zu den verschiedenen Funktionen und Optionen für das Suchen und Ersetzen von Text im Gleam-Programmierungsframework findest du in der offiziellen Dokumentation.

## Tiefergehende Informationen

Wenn du wirklich in die Details eintauchen möchtest, bietet Gleam auch die Möglichkeit, einen eigenen Parser zu schreiben, der Text analysiert und gezielt ersetzt. Mit dem `match`-Konstrukt und Pattern-Matching kannst du komplexe Muster definieren und den Text entsprechend anpassen.

## Siehe auch

- Offizielle Dokumentation zu `String.replace`: https://gleam.run/documentation/stdlib/string#replace
- Offizielle Dokumentation zu `Regex.replace`: https://gleam.run/documentation/stdlib/regex#replace
- Beispielprojekte auf Github: https://github.com/gleam-lang/gleam/tree/main/examples/search-and-replace