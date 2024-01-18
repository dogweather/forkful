---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Elixir: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän parsiminen merkkijonosta on prosessi, jossa päivämäärä muunnetaan tietokoneen ymmärtämään muotoon. Tätä tarvitaan esimerkiksi, kun halutaan käsitellä käyttäjän syöttämiä päivämääriä sovelluksissa. Päivämäärän parsinta on tärkeä osa ohjelmointia, sillä se mahdollistaa päivämäärien käsittelyn ja muokkaamisen tietokoneen avulla.

## Kuinka se tehdään?
Päivämäärän parsiminen merkkijonosta on helppoa Elixir-ohjelmointikielessä. Alla on esimerkki, jossa käytetään Date-pakettia ja sen toimintoa `parse`:

```elixir
date = Date.parse("23.05.2021", "~d.~m.~Y")
IO.inspect date
```

Tämä tuottaa seuraavan tulosteen:

```ShellSession
{:ok, ~D[2021-05-23]}
```

Pakettiin annetaan ensin parsittava merkkijono ja sen jälkeen miten päivämäärä on muotoiltu merkkijonossa "~d.~m.~Y". Tämä merkintätapa tarkoittaa, että päivämäärä on muotoa päivä.kuukausi.vuosi. Jos päivämäärä onnistuu parsimaan onnistuneesti, Date-paketti palauttaa tuplen, jossa ensimmäinen arvo on `:ok` ja toinen arvo on parsittu päivämäärä.

## Syväkatsaus
Päivämäärän parsinta on ollut tärkeä osa ohjelmointia jo pitkään. Ennen oli yleistä, että päivämäärät esitettiin useissa eri muodoissa, mikä vaikeutti niiden käsittelyä. Nykyään onneksi on olemassa erilaisia paketteja ja toimintoja, kuten Elixirin Date-paketti, jotka tekevät päivämäärien käsittelystä ja parsimisesta helppoa.

On myös olemassa muita vaihtoehtoja päivämäärän parsimiselle, kuten Ruby:ssa käytetty Date-paketti ja Pythonin datetime-moduuli. Näissä kielissä päivämäärän parsinta toteutetaan hieman eri tavalla, mutta idea on kuitenkin sama.

## Katso myös
Jos haluat lukea lisää Elixir-kielen Date-paketista, tarkista Elixirin viralliset dokumentaatiot osoitteesta https://hexdocs.pm/elixir/Date.html.

Voit myös harkita osallistumista Elixir-yhteisön keskusteluihin ja saada lisätietoa ohjelmointikielestä osoitteesta https://elixirforum.com/.

Happy coding! 🚀