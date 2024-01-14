---
title:                "Elixir: Lähettämässä http-pyyntöä perusautentikoinnilla"
simple_title:         "Lähettämässä http-pyyntöä perusautentikoinnilla"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeä osa monien ohjelmistojen toteutusta, sillä se mahdollistaa käyttäjien tunnistamisen ja pääsyn rajoitettuihin alueisiin. Tässä blogikirjoituksessa käymme läpi, miten lähetät HTTP-pyynnön Elixir-kielisen ohjelman avulla perusautentikoinnilla.

## Kuinka tehdä

Perusautentikoinnin lähettämiseen tarvitaan muutama eri vaihe. Ensimmäiseksi luodaan HTTP-pyyntö osoittamaan haluttuun sivustoon tai palveluun. Sitten meidän täytyy lisätä pyyntöön mukaan perustiedot kirjautumistiedoista. Lopuksi lähetämme pyynnön ja käsittelemme vastauksen.

```Elixir
url = "https://www.example.com"
auth = "username:password"
request = HTTPoison.get(url, headers: [{"Authorization", "Basic " <> Base.encode64(auth)}])
```

Tässä esimerkissä luomme muuttujan `url`, joka sisältää pyynnön URL-osoitteen. Sitten luomme toisen muuttujan `auth`, joka sisältää kirjautumistiedot muodossa `käyttäjätunnus:salasana`. Lopuksi lähetämme pyynnön käyttäen `HTTPoison`-kirjastoa ja lisäämme mukaan headerin, joka sisältää `Authorization`-avaimen ja `Basic`-peruksen. Tämä kodutus muuntaa `auth`-muuttujan arvon base64-muotoon, joka on välttämätön perusautentikoinnin lähettämisessä. 

HTTP-pyyntöön voi myös lisätä muita tarvittavia tietoja, esimerkiksi erilaisia parametreja tai bodyn. Nämä lisätiedot tulee lisätä `HTTPoison`-kirjaston dokumentaation mukaisesti.

## Syvemmälle

Perusautentikoinnin lähettäminen on yksi tapa, mutta se ei ole ainoa tapa lähettää autentikointitietoja HTTP-pyynnössä. Joissakin tapauksissa on tarpeen lisätä `Authorization`-headerin arvoksi muu kuin base64-muodossa oleva käyttäjänimi ja salasana. Tässä tapauksessa meidän täytyy itse hoitaa base64-koodaus ja lisätä se `Authorization`-headeriin käyttäen `'Basic ' <> koodattu_tunnukset` konstruktoria. 

Lopuksi, Elixirin tyylikäs syntaksi ja monipuoliset kirjastot tekevät HTTP-pyyntöjen lähettämisestä helppoa ja tehokasta.

## Katso myös

- [HTTPoison dokumentaatio](https://hexdocs.pm/httpoison/)
- [HTTP-pyyntöjen lähetys Elixirillä](https://pragmaticperl.com/elixir/crafting-http-requests-with-elixir/)