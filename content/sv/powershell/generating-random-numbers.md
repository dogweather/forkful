---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:49.136977-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal innebär att skapa nummer som inte följer något igenkännbart mönster. Programmerare behöver detta för tester, säkerhet och simuleringsuppgifter - tänk lösenord, spel och statistisk sampling.

## Så här gör du:
Skapa ett slumpmässigt tal mellan 1 och 100:
```PowerShell
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```
Sample output kan vara:
```
42
```
För en sekvens av slumptal:
```PowerShell
1..10 | ForEach-Object { Get-Random -Minimum 1 -Maximum 101 }
```
Output blir en lista med 10 nummer.

Skapa ett slumpmässigt tal med en mer kryptografiskt säker metod:
```PowerShell
$randomBytes = New-Object byte[] 4
[System.Security.Cryptography.RandomNumberGenerator]::Create().GetBytes($randomBytes)
$randomNumber = [BitConverter]::ToInt32($randomBytes, 0)
Write-Output $randomNumber
```
## Djupdykning
I äldre PowerShell-versioner användes ofta `Random`-klassen i .NET. Men `Get-Random` är enklare och inbyggt i PowerShell. För kryptografiska ändamål är klassen `System.Security.Cryptography.RandomNumberGenerator` rekommenderad då den genererar oförutsägbara värden, vilket är viktigt för säkerhetsrelaterade uppgifter.

Alternativ till `Get-Random` inkluderar tredjepartspaket eller att direkt använda .NET-objekt i ditt script. Men var medveten om att "slumpmässighet" i datorer inte är riktigt slumpmässig - de är pseudoslumpmässiga, baserade på algoritmer. För de flesta icke-säkerhetskritiska ändamål fungerar dessa dock väl.

## Se även
- [Get-Random documentation](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-random)
- [About Random Number Generation in .NET](https://docs.microsoft.com/dotnet/api/system.random)
- [PowerShell GitHub repository](https://github.com/PowerShell/PowerShell) för insyn i utvecklingen av PowerShell.