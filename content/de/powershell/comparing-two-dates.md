---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Der Vergleich von zwei Daten ist das Feststellen der Zeitdifferenz zwischen ihnen. Dies ist wichtig für Programmierer, um Zeit-basierte Funktionen und Abläufe zu erstellen, wie z. B. Datums-Countdowns oder Datumsgesteuerte Ereignisse.

## Wie geht das:

Im PowerShell können wir das `-gt`(größer als), `-lt`(kleiner als), `-ge`(größer oder gleich), `-le`(kleiner oder gleich), `-eq`(gleich), und `-ne`(nicht gleich) Operatoren verwenden, um zwei Daten zu vergleichen. Hier ist ein einfaches Beispiel:

```PowerShell
#Erstellen von zwei Datum-Objekten 
$Datum1 = Get-Date -Year 2020 -Month 1 -Day 1
$Datum2 = Get-Date -Year 2021 -Month 1 -Day 1

#Vergleich der Daten
$Datum1 -gt $Datum2
```

Die Ausgabe des obigen Codes wäre `$false`, weil `$Datum1`(1. Januar 2020) nicht später ist als `$Datum2`(1. Januar 2021).

## Deep Dive

In der früheren Versionen von PowerShell war es nicht möglich, zwei Datumsobjekte direkt zu vergleichen. Wir mussten zuerst das Datum in einen Timestamp umwandeln und dann die Timestamps vergleichen. Jetzt, mit der aktuellen Version von PowerShell, können wir die Direktvergleichsmethode verwenden, wie in der 'How to'-Sektion gezeigt. 

Es gibt andere Möglichkeiten, Daten in PowerShell zu vergleichen. Eine Alternative besteht zum Beispiel darin, die Datediff-Funktion in .NET zu verwenden, die jedoch weniger lesbar und umständlicher ist. 

Die Implementierung des Datumsvergleichs in PowerShell ist relativ einfach und unkompliziert. Man sollte sich jedoch bewusst sein, dass der Vergleich genau auf die Sekunde erfolgt.

## Siehe Auch

Weitere Informationen und Beispiele finden Sie unter:
- [Microsoft's PowerShell-Dokumentation zu Vergleichsoperatoren](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [StackOverflow Diskussionen über PowerShell Datum Vergleich](https://stackoverflow.com/questions/7834951/how-to-compare-dates-in-powershell)