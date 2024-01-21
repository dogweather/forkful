---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:40.775268-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen in PowerShell ermöglicht es Programmen, Daten oder Verhaltensweisen zu variieren. Diese Technik ist essentiell für Tests, Spieleentwicklung oder Sicherheitsfunktionen.

## So geht's:
Erzeugen Sie eine Zufallszahl zwischen 1 und 100:

```PowerShell
$random = Get-Random -Minimum 1 -Maximum 101
$random
```

Beispielausgabe:

```
42
```

Eine Liste mit 5 Zufallszahlen erstellen:

```PowerShell
1..5 | ForEach-Object { Get-Random -Minimum 1 -Maximum 101 }
```

Beispielausgabe:

```
17
58
43
2
99
```

## Tiefgang
PowerShell verwendet einen Pseudozufallszahlengenerator (PRNG), der eine Folge von Zahlen erstellt, die nur durch ihren Algorithmus bestimmt werden. Historisch gesehen stützen sich Computerprogramme seit den Anfängen der Programmierung auf PRNGs, oft mit Methoden wie der linearen Kongruenzmethode.

Andere Programmiersprachen bieten unterschiedliche Ansätze: In Python verwendet man `random`, in Java `Random` oder `SecureRandom` für kryptografische Sicherheit. Die Qualität der Zufälligkeit hängt stark vom gewählten Algorithmus und dessen Implementierung ab. In der Regel sind PRNGs gut für viele Anwendungen, für kryptografische Zwecke sollte man jedoch sicherere Methoden nutzen.

PowerShell bietet noch weitere Möglichkeiten für Zufallszahlen. Zum Beispiel kann `Get-Random` auch Elemente zufällig aus einem Array auswählen:

```PowerShell
$items = 'Apfel', 'Birne', 'Banane'
Get-Random -InputObject $items
```

Diese Methode ist nützlich, um zufällige Stichproben aus einer größeren Datenmenge zu ziehen.

## Siehe Auch
- [Get-Random-Dokumentation auf Microsoft Docs](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/get-random)
- [Über Zufallszahlengeneratoren bei RNGCryptoServiceProvider Klasse auf Microsoft Docs](https://docs.microsoft.com/de-de/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)