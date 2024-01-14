---
title:    "Elixir: Merkkijonon isoittaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi Elixirissä? Tämä on yleinen ohjelmointitehtävä, jota tarvitaan esimerkiksi, kun halutaan tulostaa käyttäjän antamia nimiä tai kun muotoillaan otsikoita ohjelmassa.

## Miten tehdä se

Onneksi Elixirissä on valmiina funktio, joka tekee tämän työn helpoksi. Käytämme funktiota `String.capitalize/1`, joka saa argumenttinaan merkkijonon ja palauttaa saman merkkijonon, mutta ensimmäinen kirjain isona. Katso esimerkki alla:

```Elixir
String.capitalize("kissa")
```

Tulostus:
```
"Kissa"
```

Voimme myös käyttää tätä funktiota muuttamaan isoksi useamman sanan sisältävän merkkijonon ensimmäisen sanan. Esimerkiksi:

```Elixir
String.capitalize("Hei, olen Elixir-ohjelmoija")
```

Tulostus:
```
"Hei, olen Elixir-ohjelmoija"
```

## Syvällisempi sukellus

Haluatko tietää, miten tämä funktio toimii taustalla? Käytännössä `String.capitalize/1` funktio käyttää Unicode-standardia tunnistaakseen kirjaimet ja muuttaa ensimmäisen kirjaimen isoksi. Lisäksi se osaa kohdella erityisiä merkkejä kuten ääkkösiä oikein. Jos haluat lisätietoa Unicode-standardista ja Elixirin käsittelystä, voit lukea [Elixirin dokumentaatiosta](https://hexdocs.pm/elixir/String.html#module-unicode-and-code-points) tai [Unicode-standardista](https://unicode.org/) itsessään.

## Katso myös

- [String.capitalize/1 dokumentaatio](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir Unicode ja koodipisteet](https://hexdocs.pm/elixir/String.html#module-unicode-and-code-points)
- [Unicode-standardi](https://unicode.org/)