---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Löschen von Zeichen nach einem Muster ist ein häufiges Vorgehen in der Programmierung, um unerwünschte oder störende Zeichen aus einem Text zu entfernen. Programmierer tun dies oft, um Textdaten für die Verarbeitung zu bereinigen oder zu normalisieren.

## So geht's:

Mit PowerShell können wir das sehr leicht durchführen. Es gibt viele Möglichkeiten, aber hier ist ein einfaches Beispiel:

```PowerShell 
$text = "Dies ist ein Beispieltext, der unerwünschte Zeichen enthält!!!"
$pattern = "[,!]"
$text = $text -replace $pattern, ""
```

Die Ausgabe wäre dann:

```PowerShell 
"Dies ist ein Beispieltext der unerwünschte Zeichen enthält"
```

Wir haben einfach einen Text definiert, ein Muster zum Suchen und das unerwünschte Zeichen dann durch Nichts ersetzt.

##Deep Dive:

Historisch gesehen ist das Löschen von Zeichen nach einem Muster aus vielen Programmiersprachen bekannt, obwohl die genaue Umsetzung variiert. In PowerShell benutzen wir reguläre Ausdrücke, um das Muster zu definieren, was sehr flexibel ist.

Zu den Alternativen gehört die manuelle Durchführung jedes Zeichens, eine Methode, die bei komplexeren Mustern sehr zeitaufwendig sein kann. Es gibt auch andere Befehle wie -split und -join, die in einigen Situationen möglicherweise nützlich sind.

Die Implementierungsdetails zur Funktion -replace in PowerShell sind ziemlich einfach. Der Operator -replace führt einen regulären Ausdruck Match durch und ersetzt dann jeden Treffer durch den angegebenen Text (in unserem Fall nichts).

Generell gilt: Bevorzugen Sie immer eingebaute Funktionen wie -replace anstelle von selbstgebauten Lösungen, um Zeit zu sparen und Fehler zu vermeiden.

##Siehe auch:

Mehr Informationen zu PowerShell, einschließlich Beispielen und Tutorials, finden Sie auf der offiziellen Microsoft-Dokumentationsseite: [Microsoft PowerShell Documentation](https://docs.microsoft.com/de-de/powershell)

Außerdem gibt es ausgezeichnete Ressourcen zur Arbeit mit regulären Ausdrücken in PowerShell, z.B.: [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)