---
title:                "Elixir: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Kun rakennat web-sovelluksia, sinun täytyy usein kommunikoida muiden palveluiden kanssa lähettämällä HTTP-pyyntöjä. Elixirin avulla voit tehdä tämän helposti ja tehokkaasti. 

## Miten

Voit lähettää HTTP-pyyntöjä Elixirissä käyttämällä `HTTPoison`-kirjastoa. Voit aloittaa käyttämällä `mix`-komentorivi työkalua asentaaksesi `HTTPoison`-kirjaston:

```Elixir
mix deps.get
```

Tämän jälkeen voit luoda GET-pyynnön käyttämällä `HTTPoison.get()` -funktiota ja määrittämällä haluamasi URL-osoitteen. Esimerkiksi:

```Elixir
HTTPoison.get("https://www.example.com")
```

Tämä palauttaisi sinulle `{:ok, %HTTPoison.Response{}}` -tuloksen, joka sisältää vastauksen tiedot, kuten statuskoodin ja headerit. Voit myös lähettää POST-pyynnön käyttämällä `HTTPoison.post()` -funktiota ja määrittämällä haluamasi parametrit. Esimerkiksi:

```Elixir
HTTPoison.post("https://www.example.com/api", body: %{id: 1, name: "John"})
```

## Syventävää

HTTP-pyyntöjen lähettäminen Elixirissä onnistuu myös muilla kirjastoilla, kuten `Finch` ja `Tesla`. Nämä kirjastot tarjoavat lisäominaisuuksia, kuten asynkronisen lähetyksen ja automaattisen JSON-muunnoksen. Lisäksi voit myös hyödyntää Elixirin ominaisuuksia, kuten potentiaalisesti haitallisten prosessien hallinnan, tehdäksesi HTTP-pyyntöjesi lähetyksestä luotettavampaa ja suorituskykyisempää.

## Katso myös

- [HTTPoisonin dokumentaatio](https://github.com/edgurgel/httpoison)
- [Finchin dokumentaatio](https://github.com/keathley/finch)
- [Teslan dokumentaatio](https://github.com/teamon/tesla)

Kiitos lukemisesta! Toivottavasti tämä auttoi sinua ymmärtämään, miten lähettää HTTP-pyyntöjä Elixirissä. Onnea ohjelmoinnissa!