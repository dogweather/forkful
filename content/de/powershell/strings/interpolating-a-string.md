---
date: 2024-01-20 17:51:45.604021-07:00
description: "String-Interpolation erm\xF6glicht das Einbetten von Variablen direkt\
  \ in Zeichenketten, wodurch dynamischer und lesbarer Code entsteht. Programmierer\
  \ nutzen\u2026"
lastmod: '2024-03-13T22:44:54.087936-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation erm\xF6glicht das Einbetten von Variablen direkt in\
  \ Zeichenketten, wodurch dynamischer und lesbarer Code entsteht. Programmierer nutzen\u2026"
title: Zeichenketten interpolieren
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
String-Interpolation ermöglicht das Einbetten von Variablen direkt in Zeichenketten, wodurch dynamischer und lesbarer Code entsteht. Programmierer nutzen das, um Werte einfach in Nachrichten oder Ausgaben einzufügen.

## How to (Anleitung)
In PowerShell realisierst du String-Interpolation leicht mit dem Dollarzeichen und geschweiften Klammern innerhalb eines doppelten Anführungszeichens. Hier sind ein paar Beispiele:

```PowerShell
$name = 'Welt'
$greeting = "Hallo, $name!"
Write-Output $greeting  # Ausgabe: Hallo, Welt!

$number = 47
$message = "Ich habe $number Tomaten gekauft."
Write-Output $message  # Ausgabe: Ich habe 47 Tomaten gekauft.

$price = 9.99
$priceText = "Der Preis beträgt $($price * 1.19) Euro inklusive Mehrwertsteuer."
Write-Output $priceText  # Ausgabe: Der Preis beträgt 11.8881 Euro inklusive Mehrwertsteuer.
```

Mit diesen kleinen Code-Snippets siehst du, wie Variablen leicht in einen Textstrang eingeflochten werden können.

## Deep Dive (Hintergrundwissen)
String-Interpolation gibt's schon ewig in vielen Programmiersprachen. In PowerShell wurde es durch das `$()`-Konstrukt verbessert, welches sogar die Ausführung von ganzen Ausdrücken erlaubt. Vor PowerShell 6.0 war das Backtick-Zeichen (`) die Methode der Wahl, um Zeichen zu "escapen", aber die Interpolation mit `$()` ist weitaus mächtiger.

Es gibt Alternativen, wie z. B. den Format-Operator `-f`, der eine ähnliche Funktionalität wie die String-Interpolation bietet:

```PowerShell
$name = 'Welt'
$greeting = 'Hallo, {0}!' -f $name
Write-Output $greeting  # Ausgabe: Hallo, Welt!
```

Doch die direkte Interpolation ist oft klarer und direkter. Auch hinsichtlich der Implementierung ist es so, dass interpolierte Strings in PowerShell hinter den Kulissen in einfache string.Format()-Aufrufe konvertiert werden, daher gibt es performance-technisch meist keine großen Unterschiede.

## See Also (Siehe auch)
Für weitere Informationen über die PowerShell-Programmierung und String-Manipulation kannst du diese Ressourcen anschauen:

- Die offizielle PowerShell-Dokumentation: [https://docs.microsoft.com/de-de/powershell/](https://docs.microsoft.com/de-de/powershell/)
- Über String-Interpolation in PowerShell: [https://ss64.com/ps/syntax-operators.html](https://ss64.com/ps/syntax-operators.html)

Mit diesen Ressourcen vertiefst du dein Wissen über die nützlichen Aspekte von PowerShell.
