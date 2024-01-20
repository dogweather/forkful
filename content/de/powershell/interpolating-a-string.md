---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# PowerShell String Interpolation: Ein gründlicher Überblick

## Was & Warum?

String Interpolation ermöglicht es Programmierern, Variablen direkt in Strings einzufügen. Dies macht den Code verständlicher und einfacher zu warten.

## Wie funktioniert es?

Die Verwendung von String-Interpolation in PowerShell ist einfach. Sie können Variablen direkt in doppelten Anführungszeichen `" "` einfügen:

```PowerShell
$name = "Hans"
echo "Hallo, $name"
```

Dies erzeugt diese Ausgabe:

```PowerShell
Hallo, Hans
```

Sie können auch komplexe Ausdrücke in geschweiften Klammern `{}` innerhalb der doppelten Anführungszeichen verwenden:

```PowerShell
$num1 = 5
$num2 = 10
#echo zeigt das Ergebnis auf dem Bildschirm
echo "Die Summe von $num1 und $num2 ist $($num1+$num2)"
```

Das ergibt:

```PowerShell
Die Summe von 5 und 10 ist 15
```

## Deep Dive

Historisch gesehen wurde String Interpolation in PowerShell ab Version 3.0 unterstützt, wobei Variablen direkt in Strings eingefügt werden können.

Eine Alternative zur String-Interpolation ist die Verwendung der `-f`-Operator. Aber im Gegensatz zur String-Interpolation, ist der `-f`-Operator umständlicher und macht den Code weniger leserlich.

Was die Implementierung betrifft: PowerShell behandelt Variablen in doppelten Anführungszeichen `" "` als auszudrückende Elemente. Bei Verwendung von geschweiften Klammern `{}` führt PowerShell den Ausdruck innerhalb der Klammern aus und konvertiert das Ergebnis in einen String.

## Siehe auch

Für mehr Informationen über String-Interpolation in PowerShell, besuchen Sie bitte die offizielle Dokumentation:

- [About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)