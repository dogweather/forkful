---
title:                "Verkkosivun lataaminen"
html_title:           "PowerShell: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Web-sivun lataaminen tarkoittaa sen tiedon tallentamista, joka löytyy tietyn verkkosivun takaa. Web-kehittäjät usein lataavat web-sivuja saadakseen tarvitsemiaan tietoja ja hyödyntääkseen niitä omassa työssään.

## Kuinka:

```PowerShell
# Lataa web-sivu ja tallenna se muuttujaan
$webSivu = Invoke-WebRequest -Uri "www.esimerkkisivu.com"

# Näytä web-sivun otsikko
$webSivu.ParsedHtml.title

# Näytä web-sivun kaikki linkit
$webSivu.Links | Select-Object -ExpandProperty href

# Tallenna web-sivun sisältö tiedostoon
$webSivu | Out-File -FilePath "sivunsisalto.html"
```

## Syvällisempi sukellus:

Web-sivujen lataamisella on pitkä historia, joka juontaa juurensa internetin alkuaikoihin. Nykyään on olemassa myös muita tapoja ladata web-sivuja, kuten HTTP GET, PHP ja Python -skriptit. PowerShell antaa meille mahdollisuuden ladata ja käsitellä web-sivuja yksinkertaisella ja tehokkaalla tavalla.

## Katso myös:

- [PowerShellin dokumentaatio](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/07-formatting-objects?view=powershell-7.1)
- [Lisätietoja HTTP GET -pyynnöistä](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET)
- [PHP:n avulla web-sivujen lataaminen](https://www.php.net/manual/en/function.file-get-contents.php)