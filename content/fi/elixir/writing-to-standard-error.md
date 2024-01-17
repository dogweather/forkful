---
title:                "Kirjoittaminen standardi virheille"
html_title:           "Elixir: Kirjoittaminen standardi virheille"
simple_title:         "Kirjoittaminen standardi virheille"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kirjoittaminen virheilmoitukset standarditulostusvirheeseen on tapa, jolla ohjelmoijat voivat ilmoittaa tärkeitä virheitä tai poikkeuksia suorituksessaan. Tämä mahdollistaa näiden virheiden erottamisen tavallisista tulostuksista ja helpottaa niiden jäljittämistä ja korjaamista.

## Miten:
Voit kirjoittaa standarditulostusvirheeseen Elixirillä käyttämällä `:error_logger` -moduulia ja `Logger` -pakettia. Alla on yksinkertainen koodiesimerkki:

Elixir
```elixir
:ok = Logger.error("Tämä on virheellinen viesti")
```

Tämä komento kirjoittaa virheviestin standarditulostusvirheeseen ja palauttaa `:ok` -arvon, jos kaikki menee hyvin. Tästä voit siirtyä etsimään ja korjaamaan virheen.

## Syvempää tietoa:
Kirjoittaminen standarditulostusvirheeseen on yleisesti hyväksytty tapa ilmoittaa virheitä ohjelman suorituksessa. Tämä käytäntö on myös osa Elixirin käyttämää Erlang-viestien käsittelytapaa. Vaikka tämä tapa on yleisesti hyväksytty, on myös muita tapoja käsitellä poikkeuksia, kuten `<ex> raise` ja `Process.link`-toiminnot.

## Katso myös:
- [Elixirin virallinen dokumentaatio virheiden käsittelystä](https://hexdocs.pm/elixir/1.12/Logger.html#error-logging)
- [Erlangin virallinen dokumentaatio virheiden käsittelystä](https://erlang.org/doc/man/error_logger.html)
- [Elixirin virallinen dokumentaatio poikkeusten käsittelystä](https://hexdocs.pm/elixir/1.12/Exceptions.html)