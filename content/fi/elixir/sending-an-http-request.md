---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Elixir: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on välttämätöntä monille Elixir-ohjelmoijille, sillä useimmat sovellukset tarvitsevat kommunikoidakseen muiden palveluiden kanssa. Se on myös yksi tehokkaimmista tavoista hakea tietoa verkoista ja käsitellä asynkronisia tapahtumia.

## Miten

```Elixir
# Ensimmäinen askeleesi on ottaa käyttöön HTTPoison-kirjasto Elixirissä, joka tarjoaa HTTP-rajapinnat selainkäyttöön.
defp deps do
  [{:httpoison, "~> 1.7"}]
end
```

Pyyntöjen lähettäminen on yksinkertaista HTTPoison-kirjaston avulla. Alla on yksinkertainen esimerkki GET-pyynnön lähettämisestä hakusivulle ja sen palautuksen käsittelystä:

```Elixir
HTTPoison.get("https://www.haku.fi/")
|> case do
  {:ok, %{status_code: 200, body: body}} ->
    IO.puts(body)
  {:error, _} ->
    IO.puts("Virhe hakemisessa")
end
```

Tässä tapauksessa HTTPoison palauttaa tuplen, joka sisältää pyynnön tilan ja vastauksessa olevat tiedot. Voimme käsitellä palautuksen haluamallamme tavalla, kuten tulostamalla vastauksen ruudulle tai suorittamalla muita toimintoja sen pohjalta.

## Syvällinen sukellus

HTTPoison-kirjasto on rakennettu HTTP-rajapinnan ympärille ja tarjoaa monia hyödyllisiä toimintoja, kuten mahdollisuuden määrittää otsikoita, lähettää erilaisia pyyntöjä ja käsitellä virheitä. Lisäksi se tarjoaa myös toimintoja asynkronisten pyyntöjen lähettämiseen, mikä tekee siitä erityisen hyödyllisen Elixirissä.

Elixirissä on myös muita vaihtoehtoisia kirjastoja HTTP-pyyntöjen lähettämiseen, kuten HTTPotion ja Finch. Ne tarjoavat lisää ominaisuuksia ja pystyvät vastaamaan erilaisiin tarpeisiin. Onkin kannattavaa tutustua niihin ennen lopullisen ratkaisun valitsemista.

## Katso myös

- [HTTPoison dokumentaatio](https://hexdocs.pm/httpoison/HTTPoison.html)
- [ElixirSchool - HTTP-vaeltaja](https://elixirschool.com/fi/lessons/specifics/httpoison/)
- [The Little Elixir & OTP Guidebook: Luku 7 - HTTP-Pyyntöjen Lähettäminen](https://www.goodreads.com/book/show/25568080-the-little-elixir-otp-guidebook)