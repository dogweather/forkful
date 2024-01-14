---
title:    "Elixir: Tiedostosta lukeminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Jos olet kiinnostunut ohjelmoinnista ja etsit uutta kieltä oppiaksesi, Elixir on ehdottomasti tutustumisen arvoinen vaihtoehto. Se on tehokas ja joustava ohjelmointikieli, joka pohjautuu funktionaaliseen ohjelmointityyliin. Yksi Elixirin hyödyllisistä ominaisuuksista on kyky lukea ja käsitellä tiedostoja.

## Kuinka

Elixirilla on sisäänrakennettu toiminto ```File.read/1```, joka mahdollistaa tiedostojen lukemisen. Alla olevassa esimerkissä luomme uuden tiedoston ja tallennamme siihen tekstiä. Sitten luet tiedoston sisällön ja printtaat sen konsoliin.

```elixir
# Luodaan uusi tiedosto nimeltä testi.txt ja tallennetaan siihen tekstiä
File.write("testi.txt", "Tämä on testiteksti")

# Luetaan tiedoston sisältö ja tallennetaan muuttujaan
sisalto = File.read("testi.txt")

# Printataan sisältö konsoliin
IO.puts(sisalto)
```

Tulostus:
```
Tämä on testiteksti
```

Voit myös halutessasi avata ja kirjoittaa tiedostoon käyttämällä ```File.open/2```-funktiota.

## Syvällinen sukellus

Elixirilla on monia tapoja käsitellä ja muokata tekstiä tiedostosta. ```File.read/1``` palauttaa datan binäärimuodossa, joten sitä voi käsitellä eri tavoin käyttämällä esimerkiksi ```String```-modulia.

Toinen hyödyllinen toiminto tiedoston lukemisessa on ```File.stream!/1```, joka luo <DateTime>-tyyppisen streamin, jota voidaan käyttää tiedoston sisällön läpikäymiseen ja käsittelyyn.

## Katso myös

- [Virallinen Elixir-dokumentaatio tiedostojen käsittelystä](https://hexdocs.pm/elixir/File.html)
- [Elixirin perusteiden opetusohjelma](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir-yhteisön jakama lisämateriaali](https://elixir-lang.org/resources.html)