---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Java: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstin etsiminen ja korvaaminen on yksi tavallisimmista ohjelmointitehtävistä. Sen avulla voimme muokata tekstiä haluamallamme tavalla, kuten korjata kirjoitusvirheitä tai muuttaa tietyn sanan kaikki esiintymät. Ohjelmoijat käyttävät tätä työkalua usein tehdäkseen massamuutoksia koodiin tai muokatakseen suuria määriä tekstitiedostoja.

## Miten:
```Java
String teksti = "Tämä on esimerkkiteksti";
String uusiTeksti = teksti.replace("esimerkki", "uusi");
System.out.println(uusiTeksti);
```
Tulostus:
```
Tämä on uusi teksti
```
Tässä esimerkissä käytimme String-luokan replace-metodia korvataksesi sanan "esimerkki" sanalla "uusi". Tämä toimii myös, jos etsittävä ja korvattava sana ovat osa pidempää sanaa.

```Java
String teksti = "Tämä lause sisältää useita esimerkkisanoja, joten ne kaikki korvataan";
String korvattuTeksti = teksti.replaceAll("esimerkkisana", "korvattu sana");
System.out.println(korvattuTeksti);
```
Tulostus:
```
Tämä lause sisältää useita korvatut sanat, joten ne kaikki korvataan
```
Tässä esimerkissä käytimme String-luokan replaceAll-metodia, joka korvaa kaikki sanojen esiintymät tekstissä.

## Syvällinen sukellus:
Tekstin etsiminen ja korvaaminen on ollut käytössä jo varhaisimmista ohjelmointikielistä lähtien, ja sen merkitys vain kasvaa teknologian kehittyessä. Tässä Java-esimerkkien lisäksi löytyy monia muita tapoja toteuttaa tämä toiminto, kuten Bash-skripteillä tai tekstieditorien sisäisillä hakutyökaluilla. Kannattaa tutustua eri vaihtoehtoihin ja valita itselleen sopivin.

## Katso myös:
- [Java String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Bash-skriptien käyttö tekstien muokkaamisessa](https://linuxhint.com/sed-awk-bash/)
- [TextWrangler-tekstieditorin hakutoiminto](https://www.barebones.com/products/textwrangler/)