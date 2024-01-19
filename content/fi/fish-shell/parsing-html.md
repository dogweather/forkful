---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen tarkoittaa HTML-koodin muuttamista järkeväksi rakenteeksi, jota ohjelma voi ymmärtää. Ohjelmoijat tekevät tämän tiedon eristämiseksi ja jäsennetyn datan käytön helpottamiseksi.

## Näin teet:

```Fish Shell
# Asenna html-xml-utils
sudo apt-get install html-xml-utils

# Muuntoskriptin luominen fishillä
function parse_html
    set url $argv[1]
    curl $url | hxnormalize -x
end

# Skriptin käyttö
parse_html "https://esimerkki.fi"
```

Yllä olevassa esimerkissä luodaan funktion `parse_html`, joka lataa ja jäsentää HTML-sivun käyttämällä `curl`- ja `hxnormalize`-työkaluja.

## Deep Dive:

HTML:n jäsentämisessä ei ole mitään uutta; se on ollut web-kehittäjille tarpeellista 90-luvun alkupuolelta lähtien. Vaihtoehtoja Fish Shellille ovat esimerkiksi Python, JavaScript tai Ruby, mutta Fish erottuu selkeydellään. Tässä esimerkissä käytämme `hxnormalize`-työkalua, joka on osa `html-xml-utils`-pakettia. Se ottaa syötteenä raakaa HTML-koodia ja palauttaa siitä siistin, jäsennetyn version.

## Katso myös:

1. [Fish:](https://fishshell.com/)
Suosittu yksinkertainen komentosarjan kieli.

2. [HTML-XML-utils:](https://www.w3.org/Tools/HTML-XML-utils/)
Kokoelma yksinkertaisia ohjelmia HTML- ja XML-tiedostojen käsittelyyn.

3. [Curl:](https://curl.se/)
Työkalu tiedon siirtämiseen verkkoprotokollilla.