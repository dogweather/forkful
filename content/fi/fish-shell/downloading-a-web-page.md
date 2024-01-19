---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Verkkosivun lataaminen ihmisille suunnitellun verkkoselaimen ulkopuolella on koneellista tiedon keräämistä. Ohjelmoijat tekevät tämän saadakseen tiedot käyttöönsä esimerkiksi analytiikassa tai sisällön indeksoinnissa.

## Miten:
Meidän esimerkkimme käyttää `curl` komentoa webbisivun lataamiseen. `curl` on työkalu, jolla voidaan hakea tietoja URL-osoitteista. Koko komennon kirjoitus tapahtuu seuraavasti:

```Fish Shell
function lataa_webbisivu
    set sivusto $argv[1]
    curl $sivusto > sivu.html
end
```

Voidaan kutsua `lataa_webbisivu` -funktiota antamalla sille URL-osoite, esim. Google:

```Fish Shell
lataa_webbisivu https://www.google.com
```

Tämä tallentaa Google etusivun `sivu.html` nimiseen tiedostoon.

## Sukellus syvemmälle
`curl` työkalu on ollut olemassa jo vuodesta 1997 ja se on edelleen aktiivisessa käytössä. Vaikka tässä yhteydeksessä käymme läpi sen käyttöä Fish Shell skriptissä, se on käytettävissä useimmissa ohjelmointiympäristöissä.

Vaihtoehtoisesti voit käyttää `wget` komentoa, joka tarjoaa saman toiminnallisuuden. Ero on lähinnä käytössä ja joidenkin lisäoptioiden saatavuudessa. Fish Shellissä `wget` komennon käyttö näyttää tältä:

```Fish Shell
function lataa_webbisivu
    set sivusto $argv[1]
    wget -O sivu.html $sivusto
end
```

`curl` ja `wget` suorittavat GET pyyntöjen lähettämisen HTTP protokollan yli. Käytännössä tietokanta pyytää tietoja verkkopalvelimelta, jossa verkkosivu on sijoitettu.

## Katso lisäksi
Curl komennon dokumentaatio: https://curl.haxx.se/docs/manpage.html
Wget komennon dokumentaatio: https://www.gnu.org/software/wget/manual/wget.html
Fish Shellin dokumentaatio: https://fishshell.com/docs/current/index.html
HTTP protokolla: https://fi.wikipedia.org/wiki/Hypertekstinsiirtoprotokolla