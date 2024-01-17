---
title:                "Perusautentikoinnin lähettäminen http-pyynnöllä"
html_title:           "Elixir: Perusautentikoinnin lähettäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnöllä"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lähettäminen HTTP-pyyntö perusautentikoituna tarkoittaa sitä, että pyyntö sisältää käyttäjän tunnistetiedot autentikointia varten. Tämä on yleinen tapa lähettää salasanallisia pyyntöjä turvallisesti verkkopalvelimelle ja se mahdollistaa käyttäjän tunnistamisen ja oikeiden käyttöoikeuksien antamisen.

## Miten:

```elixir
# Lähetetään HTTP-pyyntö perusautentikoinnilla käyttäen HTTPoison-kirjastoa
conn = HTTPoison.BasicAuth.get("https://example.com", "käyttäjänimi", "salasana")
# Tulostetaan vastaus
IO.puts(conn.body)
```

Esimerkkitulostus:

```
"Sinut on autentikoitu käyttäjätunnuksella käyttäjänimi" 
```

## Sukellus syvyyksiin:

Perusautentikointi on ollut käytössä jo pitkään ja se on yksi tapa turvata salasanallisilla tiedoilla olevien pyyntöjen lähettäminen. Toisinaan käytetään myös muita autentikointimenetelmiä, kuten HMAC (Hash-based Message Authentication Code) tai OAuth (Open Authorization).

Perusautentikoinnin toteutus vaihtelee eri ohjelmointikielissä ja kirjastoissa. Joissakin tapauksissa autentikointitiedot voivat olla suoraan osana pyyntöä (esim. URLin osana), kun taas toisissa ne välitetään erillisen parametrin kautta. On myös mahdollista tallentaa autentikointitiedot muuttujaan ja käyttää niitä useissa pyynnöissä samassa sovelluksessa.

## Katso myös:

- [HTTPoison - Elixir HTTP-kirjasto](https://hexdocs.pm/httpoison/HTTPoison.BasicAuth.html)
- [Perusautentikointi Wikipedia-sivulla](https://fi.wikipedia.org/wiki/Perusautentikointi)