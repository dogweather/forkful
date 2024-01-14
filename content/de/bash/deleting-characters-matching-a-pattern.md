---
title:                "Bash: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in vielen Situationen nützlich sein. Zum Beispiel, wenn man Daten bereinigen muss oder bestimmte Textabschnitte entfernen möchte. Mit Bash-Programmierung ist dies schnell und effizient möglich.

##Wie

Die Grundlage für das Löschen von Zeichen auf der Bash-Ebene ist die `sed`-Funktion. Mit dieser Funktion können Zeichenfolgen innerhalb eines Dokuments ersetzt oder gelöscht werden, basierend auf einem angegebenen Muster.

Um das Löschen von Zeichen zu demonstrieren, betrachten wir das folgende Beispiel. Angenommen, wir haben einen Text mit unerwünschten Sonderzeichen und möchten diese entfernen. Wir können das mit `sed` wie folgt tun:

```Bash
sed 's/[^a-zA-Z0-9]//g' text.txt
```

In diesem Beispiel verwenden wir das Muster `[^a-zA-Z0-9]`, das alle Zeichen außer Buchstaben und Zahlen auswählt. Mit der Option `g` können alle Vorkommnisse in der Datei entfernt werden. Das Ergebnis wird dann in der Standardausgabe ausgegeben.

Die folgende Tabelle zeigt einige weitere Beispiele für Muster, die verwendet werden können, um Zeichen zu löschen:

| Muster          | Beschreibung                                        |
|-----------------|-----------------------------------------------------|
| `[0-9]`         | Alle Zahlen entfernen                               |
| `[^ \t\r\n\f]`  | Alle Leerzeichen und Zeilenumbrüche entfernen      |
| `[a-z]+`        | Alle Kleinbuchstaben löschen                        |
| `[^!@\#$%]`     | Alle Sonderzeichen außer '!', '@', '#', '$' und '%' |

Mit diesen Beispielen können Sie nun Ihre Bash-Skripte anpassen und unerwünschte Zeichen effizient aus Ihren Texten entfernen.

##Tiefentauchen

Ein weiterer interessanter Aspekt des Löschen von Zeichen in Bash ist die Verwendung von regulären Ausdrücken. Mit regulären Ausdrücken können komplexe Muster erstellt werden, um genau die gewünschten Zeichen auszuwählen.

Zum Beispiel können wir mit dem Muster `^\d{3}-\d{2}-\d{4}$` eine amerikanische Sozialversicherungsnummer erkennen und löschen. Das Muster passt auf eine Zeichenkette mit genau 9 Ziffern, die durch Bindestriche getrennt sind. Weitere Informationen zu regulären Ausdrücken und deren Verwendung finden Sie in der offiziellen [Bash-Dokumentation](https://www.gnu.org/software/gawk/manual/html_node/Regexp.html).

#Siehe auch

- [Offizielle Bash-Dokumentation](https://www.gnu.org/software/bash/manual/)
- [Tutorial zu regulären Ausdrücken](https://www.rexegg.com/regex-quickstart.html)
- [Handbuch zur Verwendung von Sed in Bash](https://www.gnu.org/software/sed/manual/sed.html)