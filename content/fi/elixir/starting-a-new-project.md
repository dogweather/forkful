---
title:                "Elixir: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat aloittaa uuden Elixir-projektin. Ehkä haluat oppia uutta ohjelmointikieltä, laajentaa taitojasi tai työskennellä jännittävien uusien teknologioiden parissa. Riippumatta syistäsi, aloittaminen uudessa projektissa voi olla jännittävä ja palkitseva kokemus.

## Miten aloittaa

 Aloittaaksesi uuden Elixir-projektin, tarvitset Elixir-asennuksen ja tiedostonhallintajärjestelmän, kuten Gitin. Voit luoda uuden projektin seuraavilla komennoilla:

```Elixir
mix new projekti-nimi
cd projekti-nimi
```

Nyt sinulla on pohja projektillesi. Voit aloittaa koodaamisen avaamalla projektitiedoston `projekti-nimi/lib/projekti_nimi.ex` ja aloittamalla koodin kirjoittamisen.

Voit myös lisätä ulkoisia riippuvuuksia projektiisi asentamalla paketteja Hex-palvelusta.

```Elixir
def deps do
  [{:nimi, "~> versio"}]
end
```

## Syvempi sukellus

Ennen kuin aloitat uuden projektin, on tärkeää harkita projektiisi liittyviä asioita, kuten minkä tyyppinen sovellus se on, millaisia ulkoisia palveluita tarvitset ja millaisia lisäosia haluat käyttää.

Voit myös harkita käyttämään Elixirin rakenteita, kuten OTP:ta, parhaan mahdollisen suorituskyvyn ja skaalautuvuuden saavuttamiseksi.

## Katso myös

- [Elixirin virallinen sivusto](https://elixir-lang.org/)
- [Hex-pakettien hakemisto](https://hex.pm/)
- [Elixirin dokumentaatio](https://hexdocs.pm/elixir/)