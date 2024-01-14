---
title:    "Bash: Ein Datum in einen String umwandeln"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in eine Zeichenkette ist ein nützlicher Prozess für die Verwendung von Datumsangaben in verschiedenen Bereichen der Bash-Programmierung. Dadurch können Daten einfacher dargestellt und verarbeitet werden.

## Wie man es macht

Um ein Datum in eine Zeichenkette umzuwandeln, gibt es verschiedene Ansätze, je nachdem, welches Format gewünscht ist. Hier sind einige Beispiele, die die verschiedenen Optionen aufzeigen:

```Bash
# Konvertierung in das Format "Tag Monat Jahr"
date +%d.%m.%Y
# Output: 31.12.2021

# Konvertierung in das Format "Monat Tag, Jahr"
date '+%B %d, %Y'
# Output: December 31, 2021
```

Es gibt auch die Möglichkeit, ein benutzerdefiniertes Format zu erstellen, indem man die gewünschten Elemente in Anführungszeichen schreibt, zum Beispiel:

```Bash
# Konvertierung in das Format "Jahr/Monat/Tag"
date '+%Y/%m/%d'
# Output: 2021/12/31
```

Es ist auch möglich, das Datum an einen bestimmten Zeitpunkt anzupassen, indem man das Argument "-d" verwendet und ein beliebiges Datum eingibt, etwa:

```Bash
# Konvertierung des Datums am 2. Februar 2022 in das Format "Tag Monat Jahr"
date -d "2022-02-02" +%d.%m.%Y
# Output: 02.02.2022
```

Es gibt viele weitere mögliche Kombinationen und Anpassungsmöglichkeiten, die je nach Bedarf verwendet werden können.

## Tiefergehende Informationen

Die Konvertierung eines Datums in eine Zeichenkette in Bash basiert auf dem Unix-Kommando "date". Dieses Kommando akzeptiert verschiedene Parameter und ermöglicht es dem Benutzer, das Datum und das Format nach Bedarf anzupassen. Es ist wichtig zu beachten, dass die Ausgabe des Datums von der Region und Spracheinstellung des Systems abhängig sein kann.

Es gibt auch verschiedene Optionen, um das Datum zu manipulieren oder es in Echtzeit anzuzeigen. Zum Beispiel kann man das Argument "-R" verwenden, um die Ausgabe in ein bestimmtes Format zu konvertieren, das von verschiedenen Programmen wie Mail-Client oder FTP-Servern akzeptiert wird.

Insgesamt ist das Konvertieren eines Datums in eine Zeichenkette eine wichtige Fähigkeit in der Bash-Programmierung und kann für die Handhabung von Daten in verschiedenen Szenarien sehr nützlich sein.

## Siehe auch

- [Unix-Kommando "date" - manpage](https://man7.org/linux/man-pages/man1/date.1.html)
- [Bash-Befehle - offizielles Handbuch](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Date-Conversion)