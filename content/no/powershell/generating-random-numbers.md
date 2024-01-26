---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:46.823532-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Generering av tilfeldige tall i PowerShell handler om å lage nummer som ikke kan forutsies, noe som er essensielt for alt fra testing til sikkerhet. Programmerere bruker dette for å simulerere uforutsigbare hendelser eller for å generere sikre nøkler i kryptering.

## Hvordan:

I PowerShell kan du generere tilfeldige tall slik:

```PowerShell
# Generer et tilfeldig tall mellom 0 og 99
$tilfeldig = Get-Random -Minimum 0 -Maximum 100
Write-Output $tilfeldig
```

Prøvekjøring kan gi:

```
PS /> 42
```

Lag en sekvens av tilfeldige tall:

```PowerShell
# Generer 5 tilfeldige tall mellom 10 og 20
1..5 | ForEach-Object { Get-Random -Minimum 10 -Maximum 21 }
```

Dette kan returnere noe slikt:

```
PS /> 14
PS /> 17
PS /> 13
PS /> 20
PS /> 15
```

## Deep Dive

PowerShell har brukt `Get-Random` cmdlet siden versjon 2.0 for å generere pseudotilfeldige tall, basert på systemets klokke. Pseudotilfeldighet betyr at tallene virker tilfeldige, men egentlig følger en forutsigbar sekvens som starter fra en seed.

Noen alternativer til standard `Get-Random` er:

- `System.Security.Cryptography.RandomNumberGenerator`: Mer sikker, brukes i kryptografi.
- [math]::random() i .NET klassen: En annen metode for å generere tilfeldige tall.

Detaljer varierer basert på metode, men poenget er å gi en illusjon av tilfeldighet som er tilstrekkelig for de fleste applikasjoner, men kanskje ikke for høy-sikkerhetskontekster.

## See Also

- [Get-Random offisiell dokumentasjon](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.2)
- [System.Security.Cryptography.RandomNumberGenerator klasse](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0)
- [Om tilfeldige tallgeneratorer](https://en.wikipedia.org/wiki/Random_number_generation)
