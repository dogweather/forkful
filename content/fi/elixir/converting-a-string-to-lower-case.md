---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Elixir: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi muuttaa merkkijonon pieniksi kirjaimiksi? Yksinkertaisesti sanottuna, se on usein tarpeen kun käsittelemme tekstiä tai vertailemme sanoja keskenään.

## Miten tehdä

Muista ensin ottaa käyttöön Elixirin `String` -moduuli koodissasi:

```Elixir
import String
```

Sitten voit käyttää `downcase/1` -funktiota muuttaaksesi merkkijonon pieniksi kirjaimiksi:

```Elixir
string = "MOI MAAILMA!"
downcase(string) #=> "moi maailma!"
```

Voit myös käsitellä merkkijonoja suoraan merkkijonoilla:

```Elixir
"Moi maailma!" |> downcase #=> "moi maailma!"
```

## Syvemmät tiedot

Merkkijonon muuttaminen pieniksi kirjaimiksi sisältää useita vaiheita, kuten merkkien käsittely ASCII-arvojen avulla ja eri kirjoitusjärjestelmien huomioon otto. Onneksi Elixirin `downcase/1` -funktio huolehtii näistä puolestasi, joten voit keskittyä vain tekstin käsittelyyn.

## Katso myös

- [Elixirin virallinen dokumentaatio merkkijonojen käsittelystä](https://hexdocs.pm/elixir/String.html)
- [Elixirin String-moduulin lähdekoodi GitHubissa](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/string.ex)