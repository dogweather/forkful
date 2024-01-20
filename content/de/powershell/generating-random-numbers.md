---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen werden erzeugt, um Unberechenbarkeit in Anwendungen zu erzeugen. Das ist besonders nützlich für Gaming, in Sicherheitsprotokollen und in simulierte Modelle, um eine repräsentative Stichprobe zu erzeugen.

## Wie geht das?
PowerShell bietet eine einfache Methode, um Zufallszahlen zu erzeugen. Hier ist ein kurzes Beispiel:

```PowerShell
# Zufallszahl zwischen 0 und 100
$Random = Get-Random -Minimum 0 -Maximum 100
Write-Host $Random
```
Der Befehl `Get-Random` generiert eine pseudozufällige Zahl. `Write-Host` zeigt die generierte Zahl an.

## Vertiefende Informationen
Die Methode `Get-Random` von PowerShell nutzt einen pseudozufälligen Zahlengenerator. Es ist wichtig zu wissen, dass diese Zahlen nicht für sicherheitskritische Anwendungen genutzt werden sollten.

Alternativ kann man die Klasse `[System.Random]` nutzen, die Methoden für verschiedene Datentypen bietet. So erzeugt diese z.B. Zufallszahlen in Gleitkommaform:

```PowerShell
# Zufallszahl erzeugen mit System.Random
$Rand = New-Object System.Random
$Zufallszahl = $Rand.NextDouble()
Write-Host $Zufallszahl
```
Beide Methoden sind auf der Basis des Mersenne-Twisters implementiert, einem allgemein bekannten und häufig genutzten Algorithmus für die Generierung von Zufallszahlen.

## Weiterführende Links
- Mehr über `Get-Random`: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-random 
- Mehr über `[System.Random]`: https://docs.microsoft.com/dotnet/api/system.random
- Geschichte und Details des Mersenne-Twisters: https://en.wikipedia.org/wiki/Mersenne_Twister.