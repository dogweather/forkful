---
title:                "Perusautentikoinnilla http-pyynnön lähettäminen"
html_title:           "PowerShell: Perusautentikoinnilla http-pyynnön lähettäminen"
simple_title:         "Perusautentikoinnilla http-pyynnön lähettäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Lähettäminen HTTP-pyynnön perusautentikoinnin kanssa on tapa lähettää pyyntö verkkosivulle, jossa tarvitaan todennus käyttäjän vahvistamiseksi. Kehittäjät käyttävät tätä tekniikkaa varmistaakseen turvallisen ja luotettavan pääsyn sivustolle.

# Miten:

```
PowerShell
$request = [System.Net.WebRequest]::Create("url")
$credentials = New-Object System.Net.NetworkCredential("username", "password")
$request.Credentials = $credentials
$response = $request.GetResponse()
$responseStream = $response.GetResponseStream()
$reader = New-Object System.IO.StreamReader($responseStream)
$data = $reader.ReadToEnd()
```

Esimerkissa luodaan HTTP-pyyntö määritettyyn URL-osoitteeseen ja annetaan vaadittavat käyttäjän tiedot. Sitten vastaus tallennetaan muuttujaan ja luetaan vastauksen tietoja.

# Syväsukellus:

Perusautentikoinnin käyttöönotto tuli tarpeelliseksi, kun internetin käyttö lisääntyi 1990-luvulla ja turvallisen tiedonsiirron tarve kasvoi. Nykyään on olemassa muitakin tapoja lähettää HTTP-pyyntöjä, kuten digest-autentikointi ja OAuth, mutta perusautentikointi on edelleen laajasti käytössä.

Perusautentikointi toimii lähettämällä käyttäjän tiedot salattuna pyyntönä verkkosivulle, joka sitten tarkistaa nämä tiedot hyväksyäkseen pääsyn. Tämä prosessi on ollut suhteellisen turvallinen, mutta se ei enää ole suositeltava tapa lähettää salattuja tietoja.

# Katso myös:

- [HTTP-autentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
- [PowerShellin [System.Net.WebRequest]](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=netcore-3.1)
- [HTTP-pyynnön lähetys PowerShellilla](https://devblogs.microsoft.com/scripting/weekend-scripter-use-the-webrequest-net-class-to-send-http-queries)