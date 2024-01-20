---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I programmering betyr samkjøring av strenger å sette sammen to eller flere strenger til én. Dette er nødvendig når vi ønsker å kombinere tekst fra ulike kilder eller generere dynamisk tekst.

## Hvordan gjør man det:

Concatenering av strenger i PowerShell er enkelt. Bruk `+` operatøren for å koble to strenger. Her er et enkelt eksempel:

```PowerShell
$str1 = "Hallo"
$str2 = "Verden"
$fullStr = $str1 + " " + $str2
Write-Output $fullStr
```

Resultatet vil bli:
```PowerShell
Hallo Verden
```

Eller du kan bruke `-f` operatør med en format streng:

```PowerShell
$str1 = "Hallo"
$str2 = "Verden"
$fullStr = "{0} {1}" -f $str1, $str2
Write-Output $fullStr
```

Resultatet vil bli:
```PowerShell
Hallo Verden
```

## Dypdykk:

Historisk sett har mange programmeringsspråk hatt forskjellige metoder for å koble sammen strenger. I PowerShell bruker vi '+', men i tidligere versjoner av .NET, måtte vi bruke StringBuilder-klassen for effektiv strengsamkjøring. 

Alternativt til "+", kan du bruke `-f` operatøren. Den lar deg formatere strenger på en mer dynamisk og fleksibel måte. 

I implementeringen av PowerShell, vil '+' operatøren opprette en ny streng i minnet hver gang den blir brukt. Dette vil kanskje ikke være den mest effektive metoden for store datamengder. I slike tilfeller er det bedre å bruke StringBuilder-klassen.

## Se også:

For mer detaljert info om strengsamkjøring og tekstbehandling i PowerShell, sjekk ut disse ressursene:

- [How to concatenate strings efficiently in c#](https://stackoverflow.com/questions/29557/how-to-concatenate-strings-efficiently-in-c-sharp)