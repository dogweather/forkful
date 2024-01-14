---
title:                "Ruby: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää Regular Expressions?

Regular Expressions eli regex on voimakas työkalu, jolla voidaan hakea, korvata ja validoida merkkijonoja helposti ja tarkasti. Se on erityisen hyödyllinen tekstianalyysissä ja datan käsittelyssä. Joten, jos olet ohjelmoija tai joudut tekemään paljon merkkijonojen käsittelyä, regexin opetteleminen voi säästää paljon aikaa ja vaivaa.

## Ohjeet Regexin käyttöön

Regular Expressionin perusmuodossa se koostuu merkistä, jota etsitään, ja erityisistä symboleista, jotka määrittävät miten merkkiä käsitellään. Esimerkiksi, jos haluat etsiä sanaa "sika" dokumentista, voit käyttää regex-mallia `/sika/`, joka vastaa kaikkia sanoja, jotka sisältävät "sika". Tämä ei kuitenkaan rajoitu vain yhteen sanaan, vaan voit käyttää myös symboleita, kuten `+` tai `*`, jotka määrittävät kuinka monta kertaa merkki esiintyy.

```Ruby
/sea+/ =~ "seal" # tulostaa 0
/sea+/ =~ "seeeal" # tulostaa 0
/sea+/ =~ "saal" # tulostaa nil
"sea" ==~ /sea+/ # tulostaa true
```

Tässä on muutamia muita hyödyllisiä symboleita, joilla voit parantaa regexisi tarkkuutta ja monipuolisuutta:

- `^` sopii merkkijonon alkuun
- `$` sopii merkkijonon loppuun
- `.` sopii mihin tahansa merkkiin paitsi uuteen riviin
- `[]` sopii joukkoon merkkejä, esim. `[abc]` sopisi a, b tai c
- `[^]` sopii kaikkiin muihin paitsi joukkoon merkkejä
- `()` ryhmittelee lausekkeita
- `?` sopii 0 tai 1 merkkiin
- `{n}` sopii tarkalleen n kertaa esiintyvään merkkiin
- `{n,}` sopii vähintään n kertaa esiintyvään merkkiin

## Syvällinen sukellus

Vaikka regex voi olla tehokas työkalu, se voi myös olla melko monimutkainen ja vaikeasti hahmotettavissa. Yksi tapa helpottaa regexin käyttöä on käyttää online työkaluja, kuten Rubular, joka auttaa testaamaan regexiä reaaliaikaisesti.

Regexiä käyttäessä on myös tärkeää huomata, että se on herkkä kirjoitusvirheille ja väärin ymmärretyille malleille voi johtaa ei-toivottuihin tuloksiin. Siksi on tärkeää testata ja varmistaa, että regex toimii odotetulla tavalla ennen sen käyttöä tuotanto-ohjelmassa.

## Katso myös

- Rubular - http://rubular.com/
- Ruby Regular Expressions - https://www.rubyguides.com/2015/06/ruby-regex/
- Ruby Docs - https://ruby-doc.org/core-2.5.1/Regexp.html