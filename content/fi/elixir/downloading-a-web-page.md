---
title:                "Verkkosivun lataaminen"
html_title:           "Elixir: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Web-sivun lataaminen tarkoittaa tietojen hankkimista verkkosivulta ja sen tallentamista paikalliselle koneelle. Ohjelmoijat tekevät tätä usein datan keräämiseksi, analysoimiseksi tai käsittelemiseksi.

## Kuinka tehdä:

```Elixir
# Lataa web-sivu ja tallenna HTML-tiedostoksi
File.write!("sivu.html", HTTPoison.get!("https://example.com").body)
# Tulostaa latauksen tilan
IO.puts("Web-sivu ladattu!")
```

## Syvällinen sukellus:

Lataaminen web-sivuilta ei ole uusi asia, mutta Elixirin kanssa se voidaan tehdä helposti ja tehokkaasti hyödyntämällä sen toimintakykyä ja skaalautuvuutta. On myös muita tapoja ladata verkkosivuja Elixirin ulkopuolella, kuten Curl-ohjelma.

## Katso myös:

- [Elixirin virallinen dokumentaatio](https://elixir-lang.org/getting-started/introduction.html#reading-files-from-the-filesystem)
- [HTTPoison-kirjaston dokumentaatio](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Curl-ohjelman dokumentaatio](https://curl.haxx.se/docs/)