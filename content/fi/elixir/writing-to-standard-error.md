---
title:                "Kirjoittaminen standardivirheelle"
html_title:           "Elixir: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit standardivirheeseen? Koska on tärkeää huomata mahdolliset virheet ja varoittaa käyttäjiä sovelluksen suorituksen aikana.

## Miten

```Elixir
 IO.puts("Kirjoita tähän") 
```

Nyt kun tiedämme miksi haluamme kirjoittaa standardivirheeseen, voimme oppia miten tehdä se käyttämällä Elixirin `IO.puts` -funktiota. `IO.puts` tulostaa halutun viestin standardivirheeseen, eli käyttäjä näkee sen suorituksen aikana. Tässä esimerkissä tulostamme yksinkertaisen tekstin, mutta `IO.puts` hyväksyy myös muita parametreja, kuten numeroita ja taulukoita.

```
Tulostaa "Kirjoita tähän" standardivirheeseen.
```

## Syvällinen tarkastelu

Kun teemme virheitä koodissa, on tärkeää huomata ne ja saada tietoa siitä, mitä meni pieleen. Tämä on erityisen tärkeää silloin, kun meillä on käyttäjiä, jotka käyttävät sovellustamme. Kirjoittamalla standardivirheeseen voimme varoittaa käyttäjiä mahdollisista ongelmista, joita he voivat kohdata. Tämä auttaa meitä korjaamaan virheitä ja parantamaan sovellustamme.

## Katso myös

- Elixirin virallinen dokumentaatio: https://elixir-lang.org/
- Artikkeli "Debugging Elixir with IO.puts" (englanniksi): https://brainspec.com/blog/2011/09/30/debugging-elixir-with-io-puts/