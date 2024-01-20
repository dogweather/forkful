---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntöjen lähettäminen on prosessi, jossa sovelluksesi pyytää tietoja tai suorittaa toiminnon verkkopalvelimella. Ohjelmoijat tekevät tämän tiedon jakamiseksi ja tehtävien suorittamiseksi etäisäntäkoneilla.

## Miten?

### PowerShell-koodiesimerkki:
 ```PowerShell 
# Luo HTTP-pyyntö
$request = [System.Net.WebRequest]::Create('http://example.com')

# Lähetä pyyntö ja saa vastaus
$response = $request.GetResponse()

# Tulosta vastauksen tilakoodi
Write-Host $response.StatusCode 

# Liberaalaa resurssit
$response.Close()
 ```

#### Tulostusnäyte:
```PowerShell
OK
```

## Syvemmälle Sukeltaminen

Historiallisesti HTTP-pyynnöt tunnetaan ensimmäisenä menetelmänä verkkopalvelimelta tiedon hakemiseen, joka lanseerattiin HTTP:n, World Wide Webin perustan, kanssa. PowerShellissa HTTP-pyynnöt voidaan tehdä useilla tavoilla, mukaan lukien Invoke-WebRequest ja Invoke-RestMethod cmdletit, jotka voidaan suorittaa lyhyemmillä koodiriveillä, mutta lisäkehitys mahdollistaen laajemman toiminnallisuuden, kuten otsikkotiedon lisäämisen tai mukautettujen pyyntöjen tekemisen. Paljon syvemmällä, HTTP-pyyntöjen lähettäminen on todella socket-ohjelmointi, jossa luodaan TCP-yhteys palvelimelle ja lähetetään pyyntöjä.

## Lisätietoja

Invoke-WebRequest Cmdlet (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)

Invoke-RestMethod Cmdlet (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)

HTTP-profiili (https://developer.mozilla.org/fi/docs/Web/HTTP)

Syvällisempi johdatus socket-ohjelmointiin (https://www.codeproject.com/Articles/52752/A-Streamlined-Method-of-Communicating-with-Network)