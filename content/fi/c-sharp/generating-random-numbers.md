---
title:                "Satunnaislukujen generointi"
html_title:           "C#: Satunnaislukujen generointi"
simple_title:         "Satunnaislukujen generointi"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Satunnaislukujen generoiminen on tärkeä osa ohjelmointia, sillä se mahdollistaa satunnaisuuden ja vaihtelun luomisen ohjelmien toimintaan. Ohjelmoijat käyttävät satunnaislukuja esimerkiksi peleissä ja simulaatioissa, joissa halutaan eri tuloksia jokaisella suorituskerralla.

## How to:

Jos haluat generoida satunnaislukuja C# -ohjelmassa, sinun tulee käyttää Random-luokkaa ja sen Next-metodia. Esimerkiksi seuraava koodi generoi satunnaisen kokonaisluvun väliltä 1-10 ja tulostaa sen konsoliin:

```
Random random = new Random();
int randomNumber = random.Next(1, 11);
Console.WriteLine(randomNumber); // voi vaihdella välillä 1-10
```

## Deep Dive:

Satunnaislukujen generoimisen historia juontaa juurensa jo 1900-luvun alkupuolelle, mutta sen yleistyminen ohjelmointikielessä tapahtui vasta 1950-luvulla. Nykyään useimmissa ohjelmointikielissä on valmiita kirjastoja satunnaislukujen generoimiseen.

On myös olemassa muita tapoja generoida satunnaislukuja kuin Random-luokan avulla. Esimerkiksi voit käyttää Guid-luokkaa, joka tuottaa uniikkeja satunnaisia merkkijonoja. Voit myös käyttää ulkoisia kirjastoja, kuten Numerical Recipes, jotka tarjoavat erilaisia satunnaislukugeneraattoreita.

Satunnaislukujen generoiminen ei ole täysin satunnainen prosessi, vaan luvut luodaan tietyn algoritmin avulla. Jotkut algoritmit ovat toisia parempia ja tuottavat tasaisemmin ja näennäisesti satunnaisempia lukuja.

## See Also:

- [Random-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Numerical Recipes -kirjaston sivu](https://www.nr.com/)
- [Satunnaislukujen generoimisen historia](https://arxiv.org/abs/1205.4475)