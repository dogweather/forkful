---
title:                "Elixir: Muunna merkkijono pieniksi kirjaimiksi"
simple_title:         "Muunna merkkijono pieniksi kirjaimiksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi Elixir-ohjelmoinnissa? Pienet kirjaimet ovat käteviä esimerkiksi vertaillessa tekstejä, sillä ne tarjoavat tarkemmat vertailukriteerit kuin isoilla kirjaimilla.

## Miten

Merkkijonon muuntaminen pieniksi kirjaimiksi Elixir:ssä on helppoa. Käytä vain `String.downcase` funktiota ja anna muunnettava merkkijono sen argumenttina. Seuraavassa esimerkissä muutamme merkkijonon "TEKSTI" pieniksi kirjaimiksi:

```Elixir
String.downcase("TEKSTI")
```

Tämä tuottaa tuloksen "teksti". Voit myös käyttää `String.downcase/1` muotoa funktiosta, jolloin voit muuttaa useita merkkijonoja pieniksi kirjaimiksi samalla kertaa. Alla on esimerkki, jossa muutamme useita merkkijonoja kerralla:

```Elixir
String.downcase(["TEKSTI1", "TEKSTI2", "TEKSTI3"])
```

Tämä tuottaa tuloksen ["teksti1", "teksti2", "teksti3"]. Voit myös tallentaa muunnetut merkkijonot uuteen muuttujaan:

```Elixir
muunnettu_merkkijono = String.downcase("TEKSTI")
```

## Syvemmälle

Miten tämä kaikki oikeastaan toimii? Pieni kirjain vastaa ASCII-koodia 97 ja iso kirjain vastaa ASCII-koodia 65. Käytännössä `String.downcase` funktio käy läpi merkkijonon merkit ja tarkistaa jokaisen merkin ASCII-koodin. Jos merkin koodi on välillä 65-90, se muunnetaan pieneksi kirjaimeksi lisäämällä siihen 32. Muut merkit säilyvät ennallaan.

Elixir:ssä merkkijonot ovat todellisuudessa listoja merkeistä, mikä tekee niiden käsittelystä erityisen helppoa. `String.downcase` funktio muuntaa nämä merkkilistat automaattisesti pieniksi kirjaimiksi käsittelemisen aikana.

## Katso myös

- [String.downcase](https://hexdocs.pm/elixir/String.html#downcase/1)
- [ASCII](https://fi.wikipedia.org/wiki/ASCII)