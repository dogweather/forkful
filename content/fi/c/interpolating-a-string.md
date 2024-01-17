---
title:                "Merkkijonon interpolointi"
html_title:           "C: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon interpolointi tarkoittaa sen fontin vaihtamista tai esimerkiksi muutaman sanan muuttamista muotoon, joka sopii paremmin kontekstiin tai tyyliin. Ohjelmoijat käyttävät interpolointia parantaakseen merkkijonojen ulkonäköä tai selkeyttääkseen tiettyä tekstiä.

## Miten:
```C
char* name = "John";
int age = 25;
printf("Hei, olen %s ja olen %d vuotta vanha", name, age);
```

```Hei, olen John ja olen 25 vuotta vanha.```

## Syväsukellus
Merkkijonon interpolointi on ollut käytössä jo vuosikymmenten ajan ja sitä käytetään edelleen laajasti eri ohjelmointikielissä. Monet modernit ohjelmointikielet, kuten Python ja JavaScript, tarjoavat valmiita toimintoja merkkijonojen interpolointiin. C-kieli vaatii käyttäjän käyttämään printf-funktiota interpoloidakseen merkkijonoja, mutta tämä tarjoaa myös suuremman hallinnan ulostulon muodon suhteen.

## Katso myös
[The Evolution of String Interpolation](https://medium.com/launch-school/the-evolution-of-string-interpolation-c6c4d520f06b)