---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en dato til en streng er å bytte fra en datotype til tekst. Programmere gjør dette for å enklere vise datoen i brukervennlige formater.

## Hvordan gjør jeg det?

Du kan konvertere en dato til en streng i PowerShell ved å bruke `ToString()` metoden. 

```PowerShell
# Opprette en dato.
$dato = Get-Date

# Konvertere dato til streng.
$streng = $dato.ToString()

# Skriv streng.
Write-Output $streng
```

Når du kjører denne koden, får du noe sånt:

```PowerShell
onsdag 26. januar 2022 14.36.08
```

Du kan også formatere datoen som du vil ved å gi et formatargument til `ToString()`.

```PowerShell
# Formatere dato.
$formatert = $dato.ToString("yyyy-MM-dd")

# Skriv formatert dato.
Write-Output $formatert
```

Og du får dette:

```PowerShell
2022-01-26
```

## Dypdykk

Historisk sett, i tidligere versjoner av PowerShell, kunne du bruke `ToShortDateString()` eller `ToLongDateString()` for å konvertere datoer til strenger. Men i nyere versjoner, bruker vi `ToString()` for mer fleksibilitet.

Alternativt, du kan bruke `-f` operatoren for å lage en formatert streng direkte.

```PowerShell
# Bruk -f operatoren.
$formatert2 = "{0:yyyy-MM-dd}" -f $dato

# Skriv formatert dato.
Write-Output $formatert2
```

Dette gir samme resultat:

```PowerShell
2022-01-26
```

Noen detaljer om implementeringen: Når du bruker `ToString()`, konverterer PowerShell datoen til en streng basert på standard lokal innstilling, med mindre du spesifiserer et format.

## Se også 

For mer informasjon om datoformat i .NET (som PowerShell bruker), se her: [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).

For mer om PowerShell og datoer, se her: [Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1).