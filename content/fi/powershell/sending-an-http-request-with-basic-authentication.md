---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lähettäminen HTTP-pyynnön perusautentikoinnilla tarkoittaa pyynnön lähettämistä joko GET- tai POST-menetelmän avulla, johon sisältyy käyttäjän tunnistaminen salasanan ja käyttäjänimen avulla. Ohjelmoijat tekevät tämän saadakseen pääsyn suojattuihin tietolähteisiin tai webbipalveluihin.

## Näin tehdään:

Voit lähettää HTTP-pyynnön PowerShellissa käyttäen `Invoke-WebRequest` -komentoa. Esimerkiksi:

```PowerShell
# Määritä URL, käyttäjänimi ja salasana
$url = 'https://esimerkki.com'
$username = 'kayttajanimi'
$password = 'salasana'

# Luo aitouskoodi
$pair = "$($username):$($password)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))
$basicAuthValue = "Basic $encodedCreds"

# Luo pyyntö
$headers = @{
    Authorization = $basicAuthValue
}

# Lähetä pyyntö
$response = Invoke-WebRequest -Uri $url -Method Get -Headers $headers

# Tulosta vastaus
$response.StatusCode
```

Yllä oleva koodi luo HTTP GET -pyynnön, sisältäen siihen perusautentikoinnin. Vastauksenä saat HTTP-tilakoodin.

## Syvempi sukellus:

HTTP perusautentikointi on osa Hypertext Transfer Protocol (HTTP) standardia, ja se on ollut käytössä lähes sen alkuperäisestä julkaisusta lähtien. Vaihtoehtona on käyttää esim. digest-todentamista tai OAuth-autentikointimenetelmiä.

Muista, että HTTP-perustodennus lähettää salasanan Base64-koodatussa muodossa, jota ei salata, joten sitä ei pidä käyttää yli suojaamattoman yhteys. HTTPS:tä suositellaan aina.

## Katso myös:

- 'Invoke-WebRequest': [virallinen PowerShell-dokumentaatio](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- HTTP-todennus: [MDN Web Docs](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
- 'Basic access authentication': [Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)