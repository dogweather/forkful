---
title:                "Vergleich von zwei Daten"
html_title:           "PowerShell: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist eine häufige Aufgabe in der Programmierung. Es beinhaltet Überprüfung der Gleichheit, Reihenfolge oder anderen Beziehungen zwischen zwei Datumsangaben. Programmierer verwenden diese Technik, um beispielsweise Datenbankabfragen zu filtern oder Geschäftslogik zu implementieren.

## How to:
Die folgenden Beispiele veranschaulichen die Grundlagen des Vergleichens von zwei Daten in PowerShell:

```PowerShell
$date1 = Get-Date -Date "2020-08-10"
$date2 = Get-Date -Date "2020-08-12"

$date1 -eq $date2   # Gleichheit prüfen
$date1 -lt $date2   # Reihenfolge prüfen
```

Dies würde die Ausgabe ```False``` für die erste Zeile und ```True``` für die zweite Zeile zurückgeben.

Weitere Vergleichsoperatoren, die in PowerShell zur Verfügung stehen, sind:

- `-gt` (größer als)
- `-ge` (größer oder gleich)
- `-lt` (kleiner als)
- `-le` (kleiner oder gleich)

```PowerShell
$date1 = Get-Date -Date "2020-08-10"
$date2 = Get-Date -Date "2020-08-12"

$date1 -gt $date2   # Ausgabe: False
$date1 -le $date2   # Ausgabe: True
```

## Deep Dive:
Das Vergleichen von zwei Daten in der Programmierung ist eine Technik, die schon seit langem verwendet wird. Herausforderungen dabei sind die unterschiedlichen Datumsformate und -standards, die in verschiedenen Ländern und Kulturen verwendet werden. Aus diesem Grund ist es wichtig, sich mit den verschiedenen Datumsfunktionen und -formaten der verwendeten Programmiersprache vertraut zu machen.

Eine Alternative zum Vergleichen von Daten ist die Verwendung von relativen Zeitangaben, wie beispielsweise "vor 2 Wochen" oder "in 3 Monaten". Dies kann je nach Anwendungsfall eine einfachere Möglichkeit sein, mit Zeitangaben umzugehen.

In PowerShell erfolgt der Vergleich von Daten auf der Basis von .NET-Funktionen und -Klassen. Dies sorgt für eine hohe Zuverlässigkeit und Genauigkeit bei der Vergleichsoperation.

## Siehe auch:
Weitere Informationen zum Vergleichen von Daten in PowerShell finden Sie unter folgenden Links:

- [Offizielle Microsoft-Dokumentation](https://docs.microsoft.com/de-de/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7)
- [Stack Overflow Diskussionen](https://stackoverflow.com/questions/tagged/powershell+date+comparison)