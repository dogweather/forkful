---
title:                "Tekstin hakeminen ja korvaaminen"
html_title:           "TypeScript: Tekstin hakeminen ja korvaaminen"
simple_title:         "Tekstin hakeminen ja korvaaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi etsiä ja korvata tekstiä? Se voi olla hyödyllistä, kun halutaan tehdä massiivisia muutoksia ohjelmakoodiin tai tekstitiedostoihin. Sen avulla voi myös nopeasti korjata kirjoitusvirheitä tai päivittää vanhentunutta sisältöä.

## Miten

```TypeScript
// Etsii ja korvaa kaikki "vanha" tekstin "uusi" tekstiksi
const uusiTeksti = vanhaTeksti.replace(/vanha/g, "uusi");
console.log(uusiTeksti);
// Tulostaa "uusi teksti"
```

```TypeScript
// Etsii ja korvaa vain ensimmäisen esiintymän "vanha" tekstin "uusi" tekstiksi
const uusiTeksti = vanhaTeksti.replace(/vanha/, "uusi");
console.log(uusiTeksti);
// Tulostaa "uusivanha teksti"
```

```TypeScript
// Käyttäjän syöttämä teksti ja korvaava teksti
const etsittavaTeksti = prompt("Kirjoita teksti, josta haluat korvata");
const korvaavaTeksti = prompt("Kirjoita teksti, jolla haluat korvata");
const uusiTeksti = vanhaTeksti.replace(etsittavaTeksti, korvaavaTeksti);
// Tulostaa korvattavan tekstin ja korvaavan tekstin välissä olevan tekstin
console.log(uusiTeksti);
```

## Syventävä tarkastelu

Etsiminen ja korvaaminen perustuvat erilaisiin säännöllisiin lausekkeisiin (regex), jotka määrittelevät etsittävän tekstin ja sen korvaavan tekstin. Nämä säännölliset lausekkeet voivat olla hyödyllisiä myös muissa tapauksissa, kuten datan muokkaamisessa ja validoinnissa.

## Katso myös

- [MDN Regex opas](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions) 
- [Ohjeet regexien käyttämiseen TypeScriptissä](https://www.digitalocean.com/community/tutorials/js-regex-typescript)
- [TypeScriptin virallinen verkkosivusto](https://www.typescriptlang.org/)