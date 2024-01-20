---
title:                "Eine Textdatei schreiben"
html_title:           "Haskell: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Schreiben einer Textdatei bedeutet, dass du Daten in einer einfachen Formatierung in einem Textformat speicherst. Programmierer nutzen dieses Verfahren, um Informationen langfristig zu speichern oder um sie mit anderen Programmen zu teilen.

# Wie geht's?

Hier ist ein Beispiel in Haskell, wie du einen Text in eine Datei schreibst:

```Haskell
main = do
  writeFile "textdatei.txt" "Das ist ein Beispieltext."
```

Und so sollte die Ausgabe aussehen:

```
textdatei.txt: Das ist ein Beispieltext.
```

# Tiefenschärfe

Das Schreiben von Textdateien ist eine gängige Methode, um Daten zu speichern, die nicht veränderbar oder nicht strukturiert sein müssen. Eine Alternative wäre das Schreiben von Binärdateien, aber diese sind für Menschen nicht lesbar und müssen erst dekodiert werden.

Bei der Implementierung von Textdateien gibt es verschiedene Wege, wie z.B. Lazy Writing oder Buffered Writing. Letztendlich hängt die Wahl davon ab, was für dich und dein Programm am effektivsten ist.

# Siehe auch

- [Einführung zu Haskell](https://haskell.org/)