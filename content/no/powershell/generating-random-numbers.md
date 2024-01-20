---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er prosessen med å produsere tall som ikke kan forutsis logisk. Programmerere gjør dette for å simulere usikkerhet, drive statistiske simuleringer, og for å tilby unike verdier når det er nødvendig.

## Hvordan:

Å generere et tilfeldig tall i PowerShell er ganske enkelt med `Get-Random` cmdlet. Her er hvordan du gjør det: 

```PowerShell
# Generer et tilfeldig tall mellom 1 og 10
$tall = Get-Random -Minimum 1 -Maximum 10
echo "Ditt tilfeldige tall er: $tall"
```

Dette vil generere et tilfeldig tall mellom 1 og 10 og vise det i konsollen.

## Dyp Dukkering:

`Get-Random` cmdlet er en del av PowerShell siden versjon 1.0, men det har blitt betydelig forbedret siden den første versjonen.

Alternativer til `Get-Random` inkluderer bruk av .NET-klassen `System.Random` eller for mer avanserte behov - `System.Security.Cryptography.RNGCryptoServiceProvider` klassen. Begge disse alternativene krever mer kode enn `Get-Random` cmdlet, men gir også flere alternativer for tilpasning.

Imidlertid bruker `Get-Random` faktisk `System.Random` klasse internt, så det gir en mye enklere grensesnitt for de samme funksjonene.

## Se Også:

1. [Microsofts offisielle dokumentasjon for Get-Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
2. [Microsoft .NET's System.Random klasse dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
3. [Microsoft .NET's System.Security.Cryptography.RNGCryptoServiceProvider klasse dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)