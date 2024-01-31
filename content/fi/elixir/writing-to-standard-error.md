---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on kirjoituskanava virheille ja logiviesteille. Tiedot stderr:iin jos ne eivät kuulu normaaliin ohjelman tulosteeseen.

## How to:
Elixirissä stderr:iin kirjoittaminen onnistuu `IO` moduulin kautta.

```elixir
# Lähetä viesti stderr:iin
IO.puts(:stderr, "Tämä on virheilmoitus")

# Tai käytä Erlangin :io moduulia suoraan
:io.format(:standard_error, "Erlangin kautta virhe: ~s~n", ["Varoitus!"])
```

Sample output virheilmoitukselle näyttäisi tältä komentorivillä:
```
Tämä on virheilmoitus
Erlangin kautta virhe: Varoitus!
```

## Deep Dive
Stderr on osa UNIX-perinnettä, ja se on suunniteltu erottelemaan normaalit ohjelman tulosteet virhetulosteista. Elixiriä käytettäessä `IO.puts/2` ja `:io.format/3` ovat suosituimmat tavat stdout:n ja stderr:n hallintaan. Stderr soveltuu erinomaisesti, kun et halua, että virheviestit ja logit sekoittuvat ohjelman varsinaiseen outputtiin.

## See Also
- Elixirin virallisesta dokumentaatiosta löydät tarkempaa tietoa `IO`:sta [täältä](https://hexdocs.pm/elixir/IO.html).
- UNIX-standardin ja filosofian historia löytyy [GNU:n sivuilta](https://www.gnu.org/gnu/gnu-history.html).
- Lisää tietoa virheenkäsittelystä ja stderr:stä löydät [tästä artikkelista](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)).
