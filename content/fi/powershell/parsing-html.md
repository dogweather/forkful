---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

HTML:n jäsentäminen on prosessi, jossa otetaan syöttönä HTML-dokumentti ja muutetaan se metatietorakenteeksi. Ohjelmoijat käyttävät tätä prosessia yleisimmin datan louhintaan verkkosivuilta.

## Näin se tapahtuu:

PowerShell käyttää Invoke-WebRequest -komentoa HTML:n jäsentämiseen. Esimerkiksi seuraava koodinpätkä hakee sivun sisällön ja tulostaa sivun otsikon.

```PowerShell
$webRequest = Invoke-WebRequest -Uri 'https://example.com'
$webRequest.ParsedHtml.title
```

Ajon jälkeen tulosteessa näkyisi esimerkiksi sivun otsikko, kuten "Example Domain".

## Syvennys:

HTML:n jäsentämisen historiallinen yhteys: HTML on luotu merkitsemään dokumentin rakenteita, ei datarakenneita. Siksi sen jäsentäminen ohjelmoitavaan muotoon on ollut aina haaste.

Vaihtoehtoja: On muita kieliä, kuten JavaScript ja Python, jotka voivat tehdä saman.

Toteutustiedot: Invoke-WebRequest -komento palauttaa ParsedHtml ominaisuuden, joka on Internet Explorerin (IE) Document Object Modelin (DOM) ilmentymä ja se vaatii IE:n olevan asennettuna koneellesi. IE:n DOM on valmis esineiden hierarkia, joka tekee työstä miellyttävää.

## Lisää tietoa löydät:

- Microsoftin Invoke-WebRequest -komentodokumentaatio: [Link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- DuckDuckGo -haku PowerShell HTML Parsing: [Link](https://duckduckgo.com/?q=powershell+html+parsing&ia=web)
- StackOverflow keskusteluja PowerShell HTML Parsing: [Link](https://stackoverflow.com/search?q=powershell+html+parsing)