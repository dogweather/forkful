---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Fish Shell: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
"Poista merkkejä vastaava kuvio" on ohjelmointitehtävä, jossa etsitään ja poistetaan merkkijonosta kaikki merkit, jotka vastaavat tiettyä kuvioita tai sääntöjä. Tämä on yleinen tehtävä ohjelmoinnissa esimerkiksi tiedon käsittelyn tai muokkaamisen yhteydessä, ja se auttaa säästämään aikaa ja vaivaa manuaalisen työn sijaan.

## Miten:
```Fish Shell``` koodiesimerkki:

```
echo "Tervetuloa Suomeen" | tr -d "aoueiy"
```

Tulostus:
```
Trvlm Snm 
```
Tässä esimerkissä käytetään ```tr```-komentoa (lyhenne sanasta "translate"), joka poistaa kaikki merkit "a", "o", "u", "e" ja "i" tulosteesta. Huomaa, että ```|```-merkki yhdistää kaksi komentoa: ensimmäinen komento tuottaa syötteen ja toinen komento käsittelee syötettä.

## Syväsukellus:
Tämä toiminto perustuu Unix-käyttöjärjestelmän perinteiseen ```tr```-komennon toimintaan, joka on tarjolla myös muissa käyttöjärjestelmissä kuten Linuxissa ja macOS:ssä. Lisäksi on olemassa muita komentoja, kuten ```sed``` ja ```awk```, jotka voivat suorittaa samanlaisia tehtäviä. Joissakin kielissä, kuten Pythonissa, tällainen toiminto voidaan suorittaa myös sisäänrakennetuilla ominaisuuksilla ilman erillistä komentoa. Fish Shellissä tämä toiminto tehdään käyttämällä komentoa ```tr```, joka on yksi useista sisäänrakennetuista komennoista.

## Katso myös:
- [Kuinka käyttää `tr` -komentoa Fish Shellissä](https://fishshell.com/docs/current/cmds/tr.html)
- [Youtube-video: Fish Shell aloittelijoille](https://www.youtube.com/watch?v=sKmt6XR7xd8)
- [StackOverflow: Poistetaan merkkejä merkkijonosta Pythonissa](https://stackoverflow.com/questions/3939361/remove-specific-characters-from-a-string-in-python)