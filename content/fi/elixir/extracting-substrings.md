---
title:                "Alaohjelmien eristäminen"
html_title:           "Elixir: Alaohjelmien eristäminen"
simple_title:         "Alaohjelmien eristäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Substringien eristäminen on Elixir-ohjelmointikielessä tapa hakea tietyt osat merkkijonoista. Tämä voi olla hyödyllistä esimerkiksi hakualgoritmeissa tai käyttäjän syötteiden validoimisessa.

## Miten?

Elixirillä on valmiina funktio nimeltä `String.slice` joka ottaa kaksi parametria: merkkijonon ja alueen josta halutaan ottaa osa merkkijonoa. Tämä alue voidaan määritellä joko kahdella luvulla tai luvuilla ja indeksillä. Esimerkiksi:

```Elixir
String.slice("hello world", 2..8)
```

Tämä palauttaisi merkkijonon `"llo wor"`.

Toinen tapa eristää substring on käyttämällä funktiota `String.split_at`. Tämä funktio ottaa myös kaksi parametria: merkkijonon ja indeksin kohdan, jossa halutaan jakaa merkkijono kahteen osaan. Esimerkiksi:

```Elixir
String.split_at("hello world", 5)
```

Tämä palauttaisi tuple-tyyppisen rakenteen jossa ensimmäisenä arvona olisi merkkijono `"hello "` ja toisena arvona merkkijono `"world"`.

## Syvemmälle

Substringien eristäminen on ollut osa Elixirin sisäänrakennettuja funktioita alusta asti. Se on nopea ja tehokas tapa hakea tiettyjä osia merkkijonoista. Toisin kuin monissa muissa kielissä, Elixirissä merkkijonot ovat muuttumattomia, joten substringien eristäminen ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden version.

Jos etsit enemmän ominaisuuksia käsitellä merkkijonoja, voit tarkistaa Elixirin `String`-moduulin dokumentaation. On myös olemassa muita kirjastoja, kuten `Regex`, jotka tarjoavat edistyneempiä tapoja käsitellä merkkijonoja.

## Katso myös

- [Elixirin dokumentaatio substringien eristämisestä](https://hexdocs.pm/elixir/String.html#slice/2)
- [Elixirin dokumentaatio String-moduulista](https://hexdocs.pm/elixir/String.html)
- [Elixirin dokumentaatio Regex-moduulista](https://hexdocs.pm/elixir/Regex.html)