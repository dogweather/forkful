---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Web-sivun lataaminen tarkoittaa sen sisällön hankkimista verkon yli omalle koneelle. Ohjelmoijat lataavat web-sivuja automatisoidakseen tiedon keräämisen tai suorittaakseen muutoksia sivun koodissa.

## Kuinka:
Lataa web-sivu PowerShell-ohjelmointikielellä seuraavasti:

```PowerShell
# Määritellään sivun URL-osoite
$url = "http://www.example.com"

# Kutsutaan Invoke-WebRequest -komentoa ladataksemme sivun
$page = Invoke-WebRequest -Uri $url

# Tulostetaan lataamamme sivun HTML-koodi
$page.Content
```

Näytölle tulisi tulostua lataamasi sivun HTML-koodi.

## Syvempi sukellus
Historiallisesti web-sivun lataaminen oli monimutkaista ja vaati työläitä skriptejä. Nykyään PowerShellin ja muiden nykyaikaisten kieliä tarjoavien työkalujen ansiosta prosessi on yksinkertaisempi ja nopeampi.

Mitä tulee vaihtoehtoihin, on olemassa lukuisia muita tapoja ladata web-sivuja, kuten cURL tai wget, jotka molemmat ovat yhtä päteviä, mutta niiden käyttö saattaa olla monimutkaisempaa.

Implementointitietoja: latauspäätöksen täytyy ottaa huomioon monta asiaa. Jos esimerkiksi ladataan suuria tiedostoja, on otettava huomioon aika ja mahdollinen datamaksu. Lisäksi on syytä tarkistaa, salliiko web-sivun omistaja sen lataamisen.

## Katso myös
- PowerShellin Invoke-WebRequest -dokumentaatio: [https://docs.microsoft.com/en-us/powershell](https://docs.microsoft.com/en-us/powershell)
- cURL: [https://curl.haxx.se](https://curl.haxx.se)
- wget: [https://www.gnu.org/software/wget](https://www.gnu.org/software/wget)