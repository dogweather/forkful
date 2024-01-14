---
title:                "Elixir: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Elixir-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas tapa käsitellä tekstinmuokkausta ja hakua Elixir-ohjelmoinnissa. Niiden avulla voit hakea, korvata ja muokata tekstidataa erittäin tarkasti ja nopeasti. Ne ovat myös erittäin hyödyllisiä, kun työskentelet tietokannoissa ja käsittelet syötteitä järjestelmästä. 

## Kuinka käyttää säännöllisiä lausekkeita Elixirissä?

Elixir tarjoaa vahvan regex-moduulin, joka sisältää paljon hyödyllisiä työkaluja säännöllisten lausekkeiden käsittelemiseen. Voit käyttää regex-moduulia importtaamalla sen koodissa:

```Elixir
import Regex
```

### Yksinkertainen haku

Yksinkertaisin tapa käyttää säännöllisiä lausekkeita on hakea tiettyä merkkijonoa tekstistä. Tämä voidaan tehdä käyttämällä `=~` -operaattoria yhdessä halutun merkkijonon kanssa.

```Elixir
regex = ~r/world/
text = "Hello world!"

Regex.match?(regex, text) #=> true
```

### Korvaaminen

Regex-moduulilla voit myös korvata osan tekstistä toisella merkkijonolla. Tämä voidaan tehdä käyttämällä `replace` -funktiota ja antamalla halutut merkkijonot korvausparametreiksi.

```Elixir
regex = ~r/world/
text = "Hello world!"

Regex.replace(regex, text, "Universe") #=> "Hello Universe!"
```

Kaikki tapaukset hoidetaan oletusarvoisesti, mutta voit myös käyttää vaihtoparametriä rajoittamaan korvausten määrää.

### Tarkempi haku

Regex-moduulilla voit myös tehdä tarkempaa hakuja käyttämällä säännöllisiä ilmauksia. Esimerkiksi, jos haluat hakea tekstistä kaikki numerot, voit käyttää ilmausta `~r/\d+/`, joka tarkoittaa kaikkia numeroita sisältäviä jonoja.

```Elixir
regex = ~r/\d+/
text = "Today's date is 20.1.2021"

Regex.scan(regex, text) #=> ["20", "1", "2021"]
```

Voit myös antaa aloitus- ja lopetusindeksit haun rajaukselle, jos haluat hakea vain osan tekstistä.

## Syvällinen tarkastelu

Säännölliset lausekkeet voivat olla monimutkaisia ja vaikeita ymmärtää aluksi, mutta niitä käyttämällä voit saavuttaa tehokkaan tekstien käsittelyn Elixirissä. On suositeltavaa tutustua regex-moduulin dokumentaatioon ja harjoitella erilaisia säännöllisiä ilmauksia.

## Katso myös

- [Elixirin virallinen regex-dokumentaatio](https://hexdocs.pm/elixir/Regex.html)
- [Regular Expressions 101 - Työkalu säännöllisten ilmausten testaamiseen](https://regex101.com/)
- [Regexper - Graafinen työkalu säännöllisten ilmausten esittämiseen](https://regexper.com/)