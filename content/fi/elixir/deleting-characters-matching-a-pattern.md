---
title:    "Elixir: Kaavan mukaiset merkkien poistaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monesti ohjelmointia tehdessä saattaa tulla tarve poistaa merkkejä tietyllä kaavalla. Tämä voi johtua esimerkiksi tietokannan käsittelyssä tai tekstinmuokkauksessa. Tässä blogikirjoituksessa käymme läpi, kuinka voit poistaa merkkejä Elixir-ohjelmointikielellä.

## Kuinka

Elixirillä on käytössä "String" moduuli, joka sisältää erilaisia toimintoja merkkijonojen käsittelyyn. Yksi niistä on "replace" funktio, jota voidaan käyttää poistamaan merkkejä kaavalla.

```
iex> String.replace("Tervetuloa", "e", "")
"Trvtuloa"
```

Tässä esimerkissä käytämme "replace" funktiota poistamaan kaikki "e" kirjaimet merkkijonosta "Tervetuloa". Lopputuloksena saamme merkkijonon "Trvtuloa".

Voimme myös käyttää "replace" funktiota RegExp regexp-moduulin kanssa, jotta voimme määrittää tarkemman kaavan poistolle. Esimerkiksi seuraavassa koodissa poistamme kaikki numerot merkkijonosta:

```
iex> String.replace_regex("1234 Elixir", ~r/[0-9]/, "")
" Elixir"
```

## Syventävä tarkastelu

Elixirillä on myös muita tapoja poistaa merkkejä. Voit käyttää esimerkiksi "String.trim" funktiota poistamaan merkkejä merkkijonon alusta ja lopusta. Voit myös muokata merkkijonoa käyttämällä "String.uppercase" ja "String.downcase" funktioita, jotka muuttavat merkistön kirjainten koon halutuksi.

Elixirillä on myös erillisiä kirjastoja, kuten "Stringex" ja "ExString", jotka tarjoavat lisää toimintoja merkkijonojen käsittelyyn.

## Katso myös

- [Elixir String moduuli](https://hexdocs.pm/elixir/String.html)
- [Elixir RegExp moduuli](https://hexdocs.pm/elixir/RegExp.html)
- [Stringex kirjasto](https://hexdocs.pm/stringex/)
- [ExString kirjasto](https://github.com/devonestes/ex_string)