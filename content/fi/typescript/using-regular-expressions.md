---
title:                "TypeScript: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja TypeScript-ohjelmoinnissa. Ne mahdollistavat tiedon hakemisen ja muokkaamisen tarkasti ja tehokkaasti merkkijonoista. Tämä auttaa vähentämään koodin määrää ja tekee siitä selkeämpää ja helpommin ymmärrettävää. Säännölliset lausekkeet ovat välttämättömiä, kun halutaan validoida ja muotoilla käyttäjän antamia tietoja esimerkiksi lomakkeissa.

## Kuinka käyttää säännöllisiä lausekkeita TypeScriptissä

Säännöllisten lausekkeiden käyttö TypeScriptissä on helppoa. Aluksi kannattaa luoda uusi RegExp-olio, joka ottaa vastaan regexp-lausekkeen ja mahdolliset hakufiagit. 

```TypeScript
let regexp = new RegExp("haluttu lauseke", "hakuflagit");
```

Sitten voidaan suorittaa haku tietystä merkkijonosta RegExp-olion match() ja exec() -metodeilla. Match()-metodi palauttaa taulukon kaikista löydetyistä osumista, kun taas exec()-metodilla on mahdollista käydä läpi osumia yksi kerrallaan.

```TypeScript
let match = regexp.match("tarkasteltava merkkijono"); // palauttaa taulukon
let exec = regexp.exec("tarkasteltava merkkijono"); // palauttaa yhden osuman
```

RegExp-luokassa on myös muita hyödyllisiä metodeita, kuten replace(), jolla voi korvata merkkijonon osia halutuilla arvoilla.

```TypeScript
let uusiMerkkijono = regexp.replace("tarkasteltava merkkijono", "haluttu korvaava arvo");
```

## Syvällisempi sukellus säännöllisiin lausekkeisiin

Säännöllisten lausekkeiden käyttö TypeScriptissä ei rajoitu pelkästään merkkijonojen hakemiseen ja korvaamiseen. Ne tarjoavat myös erilaisia hakufiagteja, jotka antavat mahdollisuuden tehdä hakuja esimerkiksi tietyn pituisista merkkijonoista, alkaen tietystä merkistä tai sisältäen tiettyjä merkkejä. Säännöllisiä lausekkeita voi myös yhdistellä helposti erilaisin operaattorein ja luoda näin monimutkaisempia hakuja.

Esimerkiksi, hakufiagi "i" sallii hakukriteerin osuman löytyä riippumatta siitä, onko kirjain väärässä vai oikeassa kirjoitusasussa. Hakufiagilla "g" voi hakea kaikkia löydettyjä osumia, eikä vain ensimmäistä, kuten oletuksena. Näitä ja muita hakufiageja kannattaa kokeilla ja käyttää rohkeasti eri tilanteissa.

## Katso myös

- [MDN: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Expressen Regular Expression Test Tool](https://regexr.com/) (apuohjelma säännöllisten lausekkeiden testaamiseen ja luomiseen)
- [TypeScript RegExp-luokan dokumentaatio](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)