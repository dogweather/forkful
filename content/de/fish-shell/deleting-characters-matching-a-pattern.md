---
title:                "Löschen von Zeichen mit übereinstimmendem Muster"
html_title:           "Fish Shell: Löschen von Zeichen mit übereinstimmendem Muster"
simple_title:         "Löschen von Zeichen mit übereinstimmendem Muster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Löschen von Zeichen, die einer bestimmten Muster entsprechen, ist das Entfernen von Zeichen, die einem vordefinierten Muster entsprechen. Programmierer tun dies, um Text oder Daten zu bereinigen oder um spezifische Informationen zu extrahieren.

# Wie geht's?
Der Fish Shell (aktuelle Version) bietet eine einfache und effektive Möglichkeit, Zeichen basierend auf einem Muster zu löschen. Hier sind ein paar Beispiele, die zeigen, wie das geht:

```
# Beispiel 1: Löschen eines einzelnen Zeichens
echo "Hallo Welt" | tr -d "a"
# Ausgabe: Hllo Welt

# Beispiel 2: Löschen eines bestimmten Worts
echo "Hello World" | sed 's/World//g'
# Ausgabe: Hello

# Beispiel 3: Löschen von Leerzeichen
echo "Willkommen zur Fish Shell" | tr -d " "
# Ausgabe: WillkommenzurFishShell
```

# Tiefer Einblick
Das Löschen von Zeichen basierend auf einem Muster ist eine gängige Funktion in vielen Programmiersprachen und Tools. Früher wurde oft das `tr` Befehl verwendet, aber heutzutage wird häufig `sed` (Stream Editor) verwendet, da es leistungsfähiger und flexibler ist.

Eine Alternative zu dem Löschen von Zeichen ist das Ersetzen von Zeichen durch andere Zeichen, zum Beispiel mit dem `sed` Befehl `s/old/new/g`, wobei "old" durch "new" ersetzt wird.

Die Fish Shell verwendet die POSIX-spezifische Funktion `sed` für die Zeichendeletion.

# Weitere Informationen
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Stack Overflow: Delete Characters Matching A Pattern in Bash](https://stackoverflow.com/questions/25534443/delete-characters-matching-a-pattern-in-bash)