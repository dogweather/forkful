---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Søk og ersta tekst med PowerShell 7: En Hurtig og Enkel Guide

## Hva & Hvorfor?
Å søke og erstatte tekst er en prosess hvor du finner spesifike strenger i en tekst og bytter ut disse med en annen streng. Dette er viktig for programmerere da det hjelper i automatiseringen av prosesser, som å korrigere feilstavninger eller å bytte ut repetetive koder.

## Hvordan:
La oss begynne med noen enkle eksempler:

```PowerShell
$text = 'Jeg liker PowerShell!'
$text -replace 'PowerShell', 'koding'
```
Output:
```
'Jeg liker koding!'
```

Her erstatter vi "PowerShell" med "koding". Du kan også bruken `-match` for å søke etter tekst:

```PowerShell
$text = 'Jeg elsker PowerShell!'
if($text -match 'PowerShell'){Write-Host 'Funnet!'}
```
Output:
```
Funnet!
```

I dette eksemplet bruker vi `-match` for å kontrollere om "PowerShell" er i teksten, hvis den er det vil 'Funnet!' bli skrevet ut.

## Dyp Dykk
PowerShell’s `-replace` og `-match` operatører stammer fra .NET’s Regular Expression (Regex) bibliotek, som ble lansert med .NET "1.0" i 2002. Alternativene inkluderer "`-contains`" og "`-like`" operatørene, som også kan brukes til å søke i tekst, men de er ikke så fleksible som Regex. På implementasjondetaljnivå, `-replace` operatøren bruker en `.Replace()` metode fra Regex objektet, mens `-match` bruker en `.IsMatch()` metode.

## Se Også
For mer informasjon og eksempler for søk og erstatting i PowerShell, kan du besøke følgende kilder:
- Microsoft PowerShell dokumentasjon: [About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- .NET Regular Expressions (Regex): [Quick Reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)