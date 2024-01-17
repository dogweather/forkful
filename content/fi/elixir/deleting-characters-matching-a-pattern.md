---
title:                "Kuviota vastaavien merkkien poistaminen"
html_title:           "Elixir: Kuviota vastaavien merkkien poistaminen"
simple_title:         "Kuviota vastaavien merkkien poistaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Miksi me ohjelmoijat poistamme merkkejä, jotka vastaavat tietytä kaavaa? Se on yksinkertainen tapa käsitellä tekstiä Elixir-ohjelmointikielellä. Elixirin sisäänrakennettu "String" -moduuli tarjoaa monia toimintoja, jotka helpottavat tekstin käsittelyä ja manipulointia. Poistamalla merkkejä, jotka vastaavat tietyt kriteerit, voimme helposti valita haluamamme tiedot ja jättää pois tarpeettomat.

## Miten:
Poista merkkejä vastaavat toiminnon avulla voimme käyttää "String" -moduulin "replace" -funktiota. Se ottaa kaksi argumenttia: alkuperäisen merkkijonon ja muotoilun, jota haluamme korvata. Käytämme sitten regex-lausekkeita määrittääksemme mitä haluamme poistaa. Alla on yksinkertainen esimerkki:

```Elixir
str = "Tervetuloa Elixir-maailmaan!"
Regex.replace(str, ~r/Elixir/, "Hello") # tulostaa "Tervetuloa Hello-maailmaan!"
```

## Syvempään:
Poista merkkejä vastaavien toimintojen käyttäminen on tullut erittäin suosituksi Elixir-yhteisössä sen helppokäyttöisyyden ja nopeuden vuoksi. Ennen Elixir-versiota 1.6, poista merkkejä vastaavaa toimintoa ei ollut sisäänrakennettuna, mutta nykyään se on käytettävissä.

On myös muita tapoja poistaa merkkejä, kuten käyttämällä "String" -moduulin "trim" -toimintoa, joka poistaa kaikki annetut merkit merkkijonon alusta ja lopusta.

Regex-lausekkeet ovat myös erittäin voimakkaita ja monipuolisia, joten kannattaa tutustua niihin ja niiden käyttöön tekstin käsittelyssä.

## Katso myös:
- [Elixirin virallinen dokumentaatio](https://elixir-lang.org/docs.html)
- [Regex-lausekkeiden opas Elixirissa](https://hexdocs.pm/elixir/Regex.html)