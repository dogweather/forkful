---
title:                "Merkkijonon isoilla alkukirjaimilla"
html_title:           "Fish Shell: Merkkijonon isoilla alkukirjaimilla"
simple_title:         "Merkkijonon isoilla alkukirjaimilla"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Kaikkiin jono-merkistöihin kirjainten muuttaminen isoiksi, eli 'capitalizing', tarkoittaa kaikkien merkistön merkkien muuttamista suuriksi kirjaimiksi. Ohjelmoijat tekevät tämän datan yhtenäistämiseksi ja vertailun helpottamiseksi.

## Miten:
Tässä on koodiesimerkki siitä, kuinka tämä tehdään Fish Shell -kuoressa:
```fish
set lower 'hei mitä kuuluu'
set upper (string upper $lower)
echo $upper
```
Sample output:
```fish
HEI MITÄ KUULUU
```
## Syventävä sukellus
Historiallisessa kontekstissa, merkkijonojen capitalisointi tuli ensin "C" -kielestä, jossa funktio to_upper() käytettiin tähän tarkoitukseen. Vaihtoehtoisesti, jotkut ohjelmointikielet, kuten JavaScript, käyttävät toUpperCase()-metodia. Fish shell:ssa, `string upper` käytetään merkkijonon muuntamiseksi isoihin kirjaimiin. Komento ottaa yhden argumentin – alkuperäisen merkkijonon – ja palauttaa uuden merkkijonon, jossa kaikki merkit ovat isoja kirjaimia.

## Katso myös
Fish Shell Ohjekirja merkkijono-operaatioista: https://fishshell.com/docs/current/ (Finnish version not available)
Ohjelmoijan oppaita ja resursseja W3Schools-sivustosta: https://www.w3schools.com/Jsref/jsref_touppercase.asp