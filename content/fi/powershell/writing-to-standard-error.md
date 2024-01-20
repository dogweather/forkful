---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä on standard error (stderr) ja miksi siihen kirjoitetaan? Stderr on erillinen tulostevirta ohjelman virheilmoituksille. Ohjelmoijat käyttävät stderr:ää erottamaan normaalin ohjelmantulosteen ja virheilmoitukset, mikä helpottaa virheiden käsittelyä ja lokien seurantaa.

## How to:
Kirjoita stderr:iin PowerShellissa käyttäen `Write-Error`-komentoa tai ohjaamalla tulostus `Write-Host`-komennolla stderr:iin `-ForegroundColor`-parametrilla.

```PowerShell
# Stderr:iin kirjoittaminen Write-Errorin avulla
Write-Error "Tämä on virheilmoitus"

#Stderr:iin kirjoittaminen Write-Hostin avulla
Write-Host "Tämä on myös virheilmoitus" -ForegroundColor Red 1>&2
```

Esimerkkien tulostus:

```
Write-Error : Tämä on virheilmoitus
At line:1 char:1
+ Write-Error "Tämä on virheilmoitus"
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
    + FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException

Tämä on myös virheilmoitus
```

## Deep Dive:
PowerShellin ensimmäisistä versioista lähtien, stderr sallii virheiden ohjaamisen erilliseen virtaan, mikä tekee virheiden hallinnasta selkeämpää. Vaihtoehtoisen menetelmänä virhetiedon kirjoittamiseen on `$host.ui.WriteErrorLine("Virheilmoitus")`, mutta `Write-Error` ja `Write-Host` ovat yleisempiä. Käyttöympäristöstä riippuen, stderr:iin kirjoittaminen voi aktivoida erilaisia hälytyksiä tai toimintoja.

## See Also:
Powershellin dokumentaatio virheiden käsittelystä:
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#success-and-error-streams)

StackOverflow-keskusteluja stderr:n käytöstä PowerShellissa:
- [How do I write standard error output in PowerShell?](https://stackoverflow.com/questions/4995733/how-do-i-write-standard-error-output-in-powershell)