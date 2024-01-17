---
title:                "Html:n jäsentäminen"
html_title:           "PowerShell: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen (eli tietojen erottaminen HTML-koodista) on tärkeä osa ohjelmointia, sillä monissa sovelluksissa tarvitaan tiedon keräämistä ja käsittelyä verkkosivustoilta. Tämä tehdään usein HTML-muodossa olevan tiedon jäsentämisen kautta.

## Miten:
Tässä esimerkissä käytämme PowerShellia jäsentämään HTML-tiedostoa ja tulostamaan sen sisältämät tiedot. Ensimmäisenä määritämme muuttujan, joka sisältää HTML-tiedoston URL-osoitteen. Sitten käytämme "Invoke-WebRequest" komentoa ladataksemme tiedoston ja tallentaa sen muuttujaan. Lopuksi käytämme "Select-Object" komentoa valitaksemme haluamamme HTML-elementit ja tulostamme ne näytölle.

```PowerShell
$url = "http://www.example.com"
$html = Invoke-WebRequest $url
$html.ParsedHtml.getElementById("main").getElementsByTagName("h1") |
Select-Object -ExpandProperty innerHTML
```
Tämä tulostaa kaikki HTML-tiedostossa olevan "main" id:n alla olevat "h1" elementit.

## Syventyminen:
HTML:n jäsentämistä on käytetty jo pitkään verkkosivustojen tietojen keräämiseen ja käsittelyyn. Ennen PowerShellia monet ohjelmoijat käyttivät JavaScriptiä ja erilaisia web-skriptauskieliä tämän tehtävän suorittamiseen. Nykyään on myös olemassa muita vaihtoehtoja kuten Ruby ja Python, mutta PowerShell tarjoaa helpon ja tehokkaan tavan jäsentää HTML:tä.

## Katso myös:
- [PowerShellin virallinen dokumentaatio HTML:n jäsentämisestä](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-5.1)
- [HTML:n jäsentäminen käyttäen JavaScriptiä](https://www.w3schools.com/js/js_htmldom.asp)
- [Pythonin BeautifilSoup-kirjasto HTML:n jäsentämiseen](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)