---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Javascript: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännöllisten lausekkeiden käyttäminen on olennainen osa nykyaikaista ohjelmointia. Se on menetelmä, joka mahdollistaa tekstin käsittelyn ja löytämisen tietyn säännön mukaan. Näin ollen säännöllisten lausekkeiden avulla voidaan helposti etsiä ja korvata tiettyjä merkkijonoja tai suorittaa monimutkaisia tiedonhakutehtäviä. Ohjelmoijat käyttävät säännöllisiä lausekkeita tehdäkseen koodistaan tehokkaampaa ja luotettavampaa.

## Miten?
Käytettäessä säännöllisiä lausekkeita Javascriptissä, käytetään RegExp-olioita ja niiden metodeja. Alla on yksinkertainen esimerkki säännöllisen lausekkeen käytöstä. Tässä tapauksessa säännöllisen lausekkeen avulla tarkistetaan, löytyykö merkkijonosta tiettyjä numeroita:

```Javascript
let string = "Tässä on 123 testimerkkijono";
let regex = /\d+/; // \d+ tarkoittaa yhtä tai useampaa numeroa
console.log(regex.test(string)); //tulostaa true, sillä merkkijonosta löytyy numeroita
```

Tämä oli vain yksi yksinkertainen esimerkki säännöllisen lausekkeen käytöstä. Säännöllisiä lausekkeita voidaan myös käyttää esimerkiksi tarkistamaan, onko sähköpostiosoite oikeassa muodossa, suodattamaan tietyn muotoisia merkkijonoja tai etsimään tietoa suuresta tekstimäärästä.

## Syvemmälle
Säännöllisten lausekkeiden historia juontaa juurensa 1950-luvulle, mutta niiden käyttö yleistyi vasta 1980-luvulla. Nykyään ne ovat osa lähes jokaista ohjelmointikieltä. Jotkut ohjelmoijat eivät kuitenkaan pidä säännöllisten lausekkeiden lukemisesta ja kirjoittamisesta, joten on olemassa myös muita vaihtoehtoja, kuten kirjastot ja ohjelmistot, jotka tekevät säännöllisten lausekkeiden käytöstä helpompaa ja intuitiivisempaa. 

Säännöllisten lausekkeiden käyttöliittymä Javascriptissä on suhteellisen yksinkertainen. Rakentaaksesi RegExp-olion käytät RegExp-konstruktoria, joka ottaa ensimmäisenä parametrina vastaan säännöllisen lausekkeen ja toisena parametrina mahdolliset asetukset. Sitten voit käyttää erilaisia metodeja, kuten test(), exec() ja match(), säännöllisten lausekkeiden käsittelyyn.

## Lue Lisää
- [MDN Web Docs - Säännölliset lausekkeet](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101 - Säännöllisten lausekkeiden testaaja ja selityksiä](https://regex101.com/)