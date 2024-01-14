---
title:                "Elixir: Työskentele jsonin kanssa"
simple_title:         "Työskentele jsonin kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## MiksiElixir on moniparannettu ja puhtaan funktionaalinen ohjelmointikieli, joka on tullut yhä suositummaksi ohjelmoijien ja yritysten keskuudessa. JSON on yksi syy, miksi Elixir on niin voimakas työkalu kehittäjille. JSON-tiedostot ovat yleinen tapa siirtää tietoa ja Elixirin avulla voit helposti lukea, käsittellä ja luoda JSON-tiedostoja. Lisäksi Elixirin tehokkaat toiminnot tekevät siitä ihanteellisen kielen JSON-tietojen kanssa työskentelyyn.

## Kuinka tehdä sitä

JSON-tietojen lukeminen Elixirillä on helppoa. Käytä ensin `HTTPoison` kirjastoa lähettääksesi GET-pyynnön API:lle ja tallentamalla vastauksen `HTTPoison.get` komennolla.

```Elixir
response = HTTPoison.get!("https://exampleapi.com/users")
```

Sitten voit käyttää `Jason` kirjastoa muuntaaksesi vastauksen JSON-muotoon.

```Elixir
json = Jason.decode(response.body)
```

Tämän jälkeen voit käyttää `Enum` toimintoja käsittelyssä ja suodattamisessa.

```Elixir
users = json
  |> Enum.filter(fn user -> Map.get(user, "role") == "admin" end)
  |> Enum.map(fn user -> %{name: user["name"], email: user["email"]} end)

IO.inspect(users)
```

Tämä koodi suodattaa JSON-taulukosta vain käyttäjät, jotka ovat rooliltaan "admin" ja luo uuden listan käyttäjistä, jotka sisältävät vain nimen ja sähköpostiosoitteen. Lopuksi `IO.inspect` tulostaa lopputuloksen.

## Syvempi sukellus

Elixir tarjoaa myös sisäänrakennetun `Poison` kirjaston, joka tekee JSON-tiedoston luomisesta erittäin helppoa. Voit luoda JSON-tiedoston seuraavasti:

```Elixir
result = Poison.encode(%{name: "John", age: 30})
```

Tämä luo JSON-tiedoston, joka näyttää tältä: `{"name": "John", "age": 30}`. Voit myös käyttää `Poison` kirjastoa muuntamaan Elixir-tyyppisiä arvoja JSON-muotoon.

```Elixir
output = %{name: "Sara", age: 25} |> Poison.encode!
```

Tämä tuottaa seuraavanlaisen JSON-tiedoston: `{"name": "Sara", "age": 25}`.

## Katso myös

- Elixirin viralliset verkkosivut: https://elixir-lang.org/
- HTTPoison kirjaston dokumentaatio: https://hexdocs.pm/httpoison/
- Jason kirjaston dokumentaatio: https://hexdocs.pm/jason/
- Poison kirjaston dokumentaatio: https://hexdocs.pm/poison/