---
title:                "Muuttaminen merkkijonon pieniksi kirjaimiksi"
html_title:           "Elixir: Muuttaminen merkkijonon pieniksi kirjaimiksi"
simple_title:         "Muuttaminen merkkijonon pieniksi kirjaimiksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi ja mitä?

String-tyypin muuntaminen pieniksi kirjaimiksi on yksi yleisimmistä tehtävistä Elixir-ohjelmoijille. Tämä prosessi antaa mahdollisuuden vertailla merkkijonoja, etsiä tiettyjä merkkijonoja ja yleensäkin käsitellä merkkijonoja yhdenmukaisella tavalla.

## Näin teet sen:

```elixir
"STRING".downcase() #=> "string"
"Yliopisto".downcase() #=> "yliopisto"
```
Tässä yksinkertaisessa esimerkissä käytetään `.downcase()`-funktiota, joka muuttaa merkkijonon pieniksi kirjaimiksi. Voit myös käyttää `.upcase()`-funktiota, joka tekee päinvastaisen toimenpiteen, eli muuntaa merkkijonon isoiksi kirjaimiksi. 

## Syvempää pohdintaa:

Historiallinen tausta: Merkkijonojen käsittely on ollut olennainen osa ohjelmointia kaikilla ohjelmointikielillä jo vuosikymmenten ajan. Perinteisesti merkkijonoja käsiteltiin vain isoilla kirjaimilla, joten tällainen muunnos pieniksi kirjaimiksi oli tarpeen, jotta ohjelmoijat voisivat vertailla merkkijonoja oikein.

Vaihtoehtoja: On olemassa myös muita tapoja muuttaa merkkijonojen iso- tai pienikirjainmuotoa, kuten käyttämällä `.capitalize()`-funktiota, joka muuttaa vain merkkijonon ensimmäisen kirjaimen isoksi tai pikkukirjaimeksi. On tärkeää valita oikea muunnosvaihtoehto riippuen siitä, mitä haluat tehdä merkkijonoilla.

Toteutus: Elixirissä merkkijonot ovat olioita, joten näitä muunnosfunktioita voidaan kutsua suoraan merkkijono-olioista.

## Katso myös:

- Elixirin dokumentaatio: https://hexdocs.pm/elixir/String.html
- Englanninkielinen artikkeli merkkijonojen käsittelystä Elixirissä: https://medium.com/@Saunawest/the-beauty-of-working-with-strings-in-elixir-1e4a3752bfbf