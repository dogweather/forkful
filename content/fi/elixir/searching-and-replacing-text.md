---
title:                "Elixir: Tekstin etsiminen ja korvaaminen"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Kuinka moneen kertaan olet joutunut muuttamaan sanan tai lauseen tekstissäsi ja sitten huomaat, että se esiintyy edelleen jossain toisessa kohdassa? Tämä on yleinen ongelma ohjelmoinnissa, mutta ei hätää - Elixirillä sinulla on helppo tapa etsiä ja vaihtaa tekstiä nopeasti ja vaivattomasti. Lue eteenpäin ja selvitetään kuinka.

## Kuinka tehdä

Etsi ja vaihda -toiminto Elixirissä on hyvin yksinkertainen. Voit käyttää `String.replace/4` funktiota, joka ottaa neljä argumenttia. Ensimmäisenä on alkuperäinen merkkijono, jossa haluat tehdä muutoksia. Seuraava argumentti on etsittävä teksti, jonka haluat korvata. Kolmantena on muuttuva tekstimuoto, joka sisältää korvaavan tekstin. Viimeisenä on hakureittiparametri, joka määrittää minkä tyyppisiä korvauksia haluat tehdä, esimerkiksi onko se isoista ja pienistä kirjaimista riippumaton. Katso alla oleva esimerkki:

```Elixir
iex> String.replace("Tervetuloa maailmaan!", "maailmaan", "Elixir", [case: :insensitive])
"Tervetuloa Elixir!"
```

Huomaa, että funktio palauttaa uuden merkkijonon ja alkuperäinen merkkijono jää muuttumattomaksi. Voit myös käyttää regexpiä (Regular expression) hakureittinä:

```Elixir
iex> String.replace("Hei 123, laskusi on maksettu.", ~r/[0-9]+/, "viisi")
"Hei viisi, laskusi on maksettu."
```

## Syvällisempi sukellus

Elixirin String-moduulilla on muitakin hyödyllisiä funktioita hakemista ja korvaamista varten, kuten `String.replace_leading/3` ja `String.replace_trailing/3`, jotka toimivat samalla tavalla kuin `String.replace`, mutta etsivät vain merkkijonon alusta tai lopusta. Myös `"Elämä" <> "Rakkaus"` yhdistää kaksi merkkijonoa ja palauttaa "ElämäRakkaus". Ja jos haluat saada merkkijonon tietyltä alueelta, voit käyttää `String.slice/3` funktiota. Näitä kaikkia funktioita voit käyttää hakemiseen ja korvaamiseen tarvittaessa.

## Katso myös

- [Elixir String-moduuli](https://hexdocs.pm/elixir/String.html)
- [Regexp-tutoriaali Elixirissä](https://elixir-lang.org/getting-started/regexp.html)
- [Elixirin kirjasto erikoiskorvauksiin](https://hexdocs.pm/poison/1.2.0/SpecialEncodings.html)