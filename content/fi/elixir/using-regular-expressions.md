---
title:                "Säännöllisten lausekkeiden käyttäminen"
html_title:           "Elixir: Säännöllisten lausekkeiden käyttäminen"
simple_title:         "Säännöllisten lausekkeiden käyttäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressions ovat tehokkaita työkaluja, jotka auttavat tiedonkäsittelyssä, tietojen etsinnässä ja muokkaamisessa. Niiden käyttö voi säästää aikaa ja vaivaa monimutkaisten tietojen käsittelyssä.

## Miten teet sen

### Haku

```elixir
text = "Hei maailma!"
Regex.run(~r/[aeiou]/, text) 
```

Tämä pieni koodinpätkä etsii tekstistä "Hei maailma!" kaikki vokaalit, jotka on merkitty sulkujen väliin. Lopputuloksena on lista kohteista, joihin osuivat vokaalit.

### Korvaaminen

```elixir
text = "Tämä on merkittävä lause."
Regex.replace(~r/merkittävä/, text, "upea")
```

Tässä korvaamme tekstistä "Tämä on merkittävä lause." sanan "merkittävä" sanalla "upea". Lopputuloksena on "Tämä on upea lause."

### Ryhmät ja takaisinviitteet

```elixir
text = "Puhelinnumero: 123-456-7890"
Regex.scan(~r/([0-9]{3})-([0-9]{3})-([0-9]{4})/, text)
```

Tämä koodinpätkä ryhmittelee 123-456-7890-numeron osiin ja palauttaa listan näistä ryhmistä. Eli esimerkissä meillä on lista numerosta, jonka jokainen osa on erilainen luku.

## Syvenny siihen

Regular expressions tarjoavat paljon muokkausmahdollisuuksia aina yksinkertaisista hauista monimutkaisiin korvaamisiin ja ryhmittelyihin. Niiden käyttöä voi laajentaa myös käyttämällä soveltuvia funktioita ja metodeja. Myös niiden käytön optimaalisuutta voi parantaa sääntöjen muuntamisella.

## Katso myös

- Elixirin dokumentaatio: https://hexdocs.pm/elixir/master/Regex.html
- RegExr: https://regexr.com/
- Regular expression cheat sheet: https://ihateregex.io/cheatsheet