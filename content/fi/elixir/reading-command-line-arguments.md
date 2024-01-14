---
title:    "Elixir: Komentoriviparametrien lukeminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmointi on jännittävää ja luovaa, mutta joillakin aloilla se voi tuntua haastavalta. Tässä blogiartikkelissa tutustumme Elixir-ohjelmointikielen tapaan lukea komentoriviparametreja ja miten se voi helpottaa ohjelmointiprosessia.

## Miten

Komentoriviparametrit ovat hyödyllisiä ohjelmien suorittamisessa, koska ne antavat käyttäjälle mahdollisuuden antaa tietoja ohjelmalle ennen sen käynnistämistä. Elixir tarjoaa helpon tavan lukea näitä parametreja käyttämällä ```System.argv()``` -funktiota. Katsotaanpa esimerkki:

```
Elixir hello.exs John 25
```

Tässä esimerkissä luomme yksinkertaisen ohjelman, joka tulostaa käyttäjän nimen ja iän. Käytämme ```System.argv()``` -funktiota lukeaksemme parametrit ja tallentamme ne muuttujiin. Sitten vain tulostamme halutut tiedot. Alla on koodiksi kommentoitu versio:

```
# "Hello, name! You are age years old."
defmodule Hello do
  # Luodaan muuttujat name ja age ja asetetaan niihin komentoriviparametrit
  name = System.argv() |> Enum.at(0)
  age = System.argv() |> Enum.at(1)

  # Tulostetaan haluttu viesti
  IO.puts "Hello, #{name}! You are #{age} years old."
end
```

Ja tässä on, miten tulostus näyttää komentoriviltä:

```
Hello, John! You are 25 years old.
```

## Syvällisempää tietoa

Luettuaan komentoriviparametreja Elixir-ohjelmilla on lisäetuna mahdollisuus käyttää OptionParser-moduulia. Tämä moduuli tarjoaa strukturoituja tapoja lukea ja hallita käyttäjän antamia parametreja. Voit tutustua tarkemmin OptionParserin käyttöön Elixirin dokumentaatiosta.

## Katso myös
- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/System.html#argv/0)
- [OptionParser-moduulin käyttö](https://hexdocs.pm/elixir/OptionParser.html#content)