---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tulosta debug-tulostus tarkoittaa erityisen viestin tulostamista ohjelman suorituksen aikana auttamaan virheenjäljityksessä. Ohjelmoijat käyttävät sitä ymmärtääkseen paremmin koodinsa toiminta logiikka ja tunnistamaankseen mahdolliset ongelmat tai virheet.

## Kuinka tehdään:

PowerShell tarjoaa Write-Debug -komennon debug-tulostuksen tulostamiseen. Tässä on perusesimerkki:

```PowerShell
$debugPreference="Continue"
Write-Debug "Tämä on debug-viesti"
```

Tämä skripti asettaa `$debugPreference`-muuttujan arvoksi "Continue", joka sallii debug-viestien tulostamisen. Skripti sitten tulostaa debug-viestin "Tämä on debug-viesti".

## Syväluotaus

Historiallisissa yhteyksissä debug-tulostus oli ensisijainen keino ohjelman toiminnan seuraamiseen. Nykyään käytettävissä olevat debuggerit tarjoavat tehokkaampia työkaluja, mutta debug-tulostus on edelleen välttämätön ohjelmoijan työkalu.

Vaihtoehtoisesti PowerShellissä voidaan käyttää Write-Verbose tai Write-Information -komentoja tiedon ulostuloon. Käytäntö riippuu ohjelmoijan Henkilökohtaisesta mieltymyksestä ja tarpeesta.

PowerShellissä debug-viestit tulostuvat vain, jos `$debugPreference`-muuttuja on asetettu sopivasti. Tämä suojaa käyttäjiä tahattomasti näkemästä häiritseviä debug-viestejä.

## Katso myös

- Lisätietoja Write-Debug -komennon käytöstä: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug
- Lisätietoja $debugPreference-muuttujan käytöstä: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables
- Vertailu Write-Debug, Write-Verbose ja Write-Information -komentojen välillä: https://www.red-gate.com/simple-talk/sysadmin/powershell/comparing-powershell-commands-write-debug-write-verbose/