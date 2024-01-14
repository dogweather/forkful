---
title:    "Elixir: Löydä merkkijonon pituus"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Merkkijonon pituuden selvittäminen voi olla hyödyllistä monissa tilanteissa, kuten tiedonkäsittelyssä, analyysissä tai muotoilussa. Se auttaa myös ymmärtämään ja manipuloimaan dataa paremmin.

## Kuinka tehdä

Voit selvittää merkkijonon pituuden Elixirilla käyttämällä `String.length()` -funktiota. Seuraavassa esimerkissä näytämme, kuinka voit käyttää tätä funktiota ja saat sen tulostamaan merkkijonon pituuden:

```Elixir
string = "Tämä on esimerkkimerkkijono"
length = String.length(string)
IO.puts length
```

Tämä tulostaa "26". Voit myös käyttää `length()` -funktiota syntaksin `&` funktiomuokkaajana ja tiivistää koodiasi seuraavasti:

```Elixir
string = "Tämä on esimerkkimerkkijono"
IO.puts string |> String.length() 
```

## Syvemmälle

Vaikka `String.length()` on Elixirin tarjoama helppo tapa selvittää merkkijonon pituus, on myös syytä tietää, että merkkijonon pituuden laskemiseen voi vaikuttaa myös kielen käyttämä merkistö. Esimerkiksi jos käytät UTF-8-merkistöä, jossa jokainen merkki voi olla yhden, kahden tai jopa neljän tavun mittainen, merkkijonon pituus ei vastaa välttämättä sen näkyvää pituutta. Tämä johtuu siitä, että `String.length()` laskemassa tavuina, ei merkkeinä.

Toinen tärkeä asia otettavaksi huomioon on, että Elixirin merkkijonot ovat muuttumattomia, mikä tarkoittaa sitä, että niitä ei voi muokata suoraan. Joten jos haluat esimerkiksi lyhentää merkkijonoa, sinun täytyy luoda uusi merkkijono ja antaa sille haluttu osa alkuperäisen merkkijonon arvosta.

## Katso myös

* [Elixirin viralliset dokumentaatiot merkkijonojen käsittelystä](https://hexdocs.pm/elixir/String.html)
* [Miten arvioida merkkijonojen käsittelystä johtuvia suorituskykyeroja Elixirissa](https://bridgetek.com/blog/2018/07/string-handling/)