---
title:                "Kaavan mukaisten merkkien poistaminen"
html_title:           "Fish Shell: Kaavan mukaisten merkkien poistaminen"
simple_title:         "Kaavan mukaisten merkkien poistaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Kuvitellaan tilanne, jossa haluat poistaa kaikki tiettyä kuviota vastaavat merkit. Tämä voi olla esimerkiksi tarpeellista jos haluat puhdistaa tietoa, joka sisältää turhia tai ei-toivottuja merkkejä. Fish Shell tarjoaa helpon ja tehokkaan tavan tehdä tämä.

## Kuinka se tehdään

Fish Shellilla voit poistaa merkkejä, jotka vastaavat tiettyä kuviota käyttämällä `string_match` komentoa ja `^` merkkiä. Tämä merkki symboloi kaikkia merkkejä, joita haluat poistaa. Alla on yksi esimerkki siitä, kuinka tätä käytetään:

```
Fish Shell koodi:
set text "Tämä on teksti, jossa on turhia merkkejä!"
echo $text

Viimeistä kertaa käyttäen "FISH regexp":
set text (echo $text | string_match -r '^[oliu].*')
echo $text

Output:
Tm vittekj yh turia merekii!
```

## Syventymistä

`string_match` komento tukee myös muita regexp-merkkejä, jotka voit käyttää määrittämään tarkemmin mitkä merkit poistetaan. Esimerkiksi `[a-z]` poistaa kaikki pienet kirjaimet tai `[1-9]` poistaa kaikki numerot. Voit myös yhdistää useampia merkkejä toisiinsa esimerkiksi `[a-z0-9]` poistaa kaikki pienet kirjaimet ja numerot.

Voit myös käyttää `string_sub` komentoa, joka antaa enemmän joustavuutta kuinka haluat poistaa merkkejä. Tätä komentoa käytetään seuraavasti: `string_sub 'merkit jotka haluat poistaa' 'mitkä merkit haluat korvata'`. Tässä komennossa voit määrittää, mitkä merkit haluat poistaa tai korvata haluamillasi merkeillä.

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Regex-opas](https://www.regular-expressions.info/fish.html)