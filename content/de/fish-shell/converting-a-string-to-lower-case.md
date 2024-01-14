---
title:    "Fish Shell: Umwandeln einer Zeichenkette in Kleinbuchstaben"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren einer Zeichenkette in Kleinbuchstaben ist eine häufige Aufgabe in der Programmierung, insbesondere wenn es um die Benutzereingabe geht. Mit Fish Shell können Sie ganz einfach eine Zeichenkette in Kleinbuchstaben umwandeln, um sie weiter zu verarbeiten.

## Wie geht das?

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, können Sie in Fish Shell die `string tolower` Funktion verwenden. Hier ist ein Beispielcode, der den Eingabetext in Kleinbuchstaben umwandelt und dann ausgegeben wird:

```
set text "Hallo Welt"
echo $text | string tolower
```

Dieses Ergebnis würde `hallo welt` ausgeben. Beachten Sie, dass die ursprüngliche Zeichenkette `Hallo Welt` unverändert bleibt, da Fish Shell Strings als unveränderbare Werte betrachtet.

## Tiefer gehend

Manchmal müssen Sie möglicherweise eine Zeichenkette in Kleinbuchstaben umwandeln und dann weiter damit arbeiten, ohne die ursprüngliche Zeichenkette beizubehalten. In diesem Fall können Sie die `set` Funktion verwenden, um den konvertierten Wert einer neuen Variablen zuzuweisen. Hier ist ein Beispielcode:

```
set text "Hallo Welt"
set lowercase_text (echo $text | string tolower)
echo $lowercase_text
```

Dieses Ergebnis würde immer noch `hallo welt` ausgeben, aber die ursprüngliche Zeichenkette `Hallo Welt` würde nun durch `lowercase_text` ersetzt.

## Siehe auch

- [Offizielle Fish Shell Dokumentation zur `string tolower` Funktion](https://fishshell.com/docs/current/cmds/string-tolower.html)
- [Andere nützliche Fish Shell Befehle für die Zeichenkettenverarbeitung](https://fishshell.com/docs/current/commands.html#string-operations)