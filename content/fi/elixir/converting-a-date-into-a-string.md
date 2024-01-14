---
title:    "Elixir: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi?

Elixir on dynaaminen ohjelmointikieli, joka on erittäin suosittu käytettäessä hajautettuja ja skaalattavia sovelluksia. Yksi sen monista hyödyllisistä ominaisuuksista on kyky muuntaa päivämäärä merkkijonomuotoon. Tässä blogipostissa käymme läpi, miten tämä onnistuu ja miksi tämä taito voi olla hyödyllinen Elixir-ohjelmoijille.

## Miten?

Muuntaaksesi päivämäärän merkkijonomuotoon Elixirissä, käytä funktiota `to_string` ja anna sille päivämäärä muodossa `{:calendar, päivämäärä}`. Katso alla oleva esimerkki:

```Elixir
date = {:calendar, {2020, 4, 14}}
date_string = to_string(date)
```

Tässä esimerkissä päivämäärä muuttuu merkkijonoksi "2020-04-14".

Voit myös lisätä haluamasi muodon käyttämällä funktiota `Date.to_string` ja antamalla sille päivämäärä ja muodon:

```Elixir
date = Date.utc_today()
date_string = Date.to_string(date, "{YYYY}-{MM}-{DD}")
```

Tässä esimerkissä käytämme `Date.utc_today` -funktiota saadaksemme nykyisen päivämäärän ja muuntamme sen haluamaamme muotoon "YYYY-MM-DD", joka antaa meille esimerkiksi "2020-04-14".

## Syvempi sukellus

Miksi sitten haluaisimme muuntaa päivämäärän merkkijonomuotoon? Usein tämä taito on hyödyllinen, kun haluamme esittää päivämäärän käyttäjälle tai tallentaa sen tietokantaan merkkijonona. Tämä voi myös auttaa muokkaamaan ja käsittelemään päivämääriä helpommin Elixir-sovelluksissasi.

Voit myös muuntaa päivämäärän merkkijonosta takaisin päivämääräksi käyttämällä `String.to_date` -funktiota ja antamalla sille päivämäärän muodossa "YYYY-MM-DD", kuten alla olevassa esimerkissä:

```Elixir
date_string = "2020-04-14"
date = String.to_date(date_string)
```

Tämä tuo takaisin päivämäärän muodossa `{:calendar, päivämäärä}`.

## Katso myös

- [Elixir-kirjaston dokumentaatio](https://hexdocs.pm/elixir/Kernel.html#to_string/1)
- [Muotoiluoptiot päivämäärän muuntamiseen](https://hexdocs.pm/elixir/1.10/Date.html#to_string/2)
- [String-moduulin dokumentaatio](https://hexdocs.pm/elixir/String.html#to_date/1)