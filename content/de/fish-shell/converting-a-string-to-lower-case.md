---
title:                "Fish Shell: Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals versucht hast, mit einem Computer zu kommunizieren, dann hast du auch schon mit Strings gearbeitet. Strings sind einfach nur eine Aneinanderreihung von Buchstaben, Zahlen oder Symbolen. Manchmal müssen wir aber diesen String in ein bestimmtes Format bringen, zum Beispiel in Kleinbuchstaben. Hier kommt die Funktion zur Konvertierung in lowercase ins Spiel.

## Wie man dies in Fish Shell macht

```Fish Shell
# String definieren
set text "Hallo, Welt!"

# Konvertiere in lowercase
echo $text | tr '[:upper:]' '[:lower:]'
```

Das Ergebnis dieser Code-Beispiele wäre "hallo, welt!". Wie du siehst, verwenden wir hier die Funktion "tr", um alle Großbuchstaben in Kleinbuchstaben zu ändern. Dabei wird der gesamte String, der als Argument übergeben wird, in Kleinbuchstaben umgewandelt.

## Tieferer Einblick

Die Funktion "tr" ist sehr nützlich, wenn man Strings manipulieren will. Es gibt jedoch noch andere Möglichkeiten, um einen String in Kleinbuchstaben zu konvertieren. Man kann zum Beispiel die "-l" Option in Kombination mit dem Befehl "echo" verwenden, um alle Buchstaben in Kleinbuchstaben auszugeben.

```Fish Shell
# String definieren
set text "Hallo, Welt!"

# Alle Buchstaben ausgeben in Kleinbuchstaben
echo -l $text
```

Ein weiterer weg, um alle Buchstaben in Kleinbuchstaben zu konvertieren ist die Verwendung der Funktion "string tolower". Diese Funktion hat dieselbe Funktionalität wie die oben erwähnten Methoden, ist jedoch spezifisch für die Konvertierung in Kleinbuchstaben.

```Fish Shell
# String definieren
set text "Hallo, Welt!"

# Konvertiere in lowercase
string tolower $text
```

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Tutorial zur Verwendung von String-Funktionen in Fish Shell](https://dev.to/chrisschuld/string-functions-in-fish-shell-3mm4)
- [Übersicht über die "tr" Funktion in Fish Shell](https://fishshell.com/docs/current/cmds/tr.html)