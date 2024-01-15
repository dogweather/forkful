---
title:                "Substringien erottelu"
html_title:           "Elixir: Substringien erottelu"
simple_title:         "Substringien erottelu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi
Oletko koskaan törmännyt tarpeeseen erottaa tietynlainen osa merkkijonosta? Ehkä haluat saada vain osan sähköpostiosoitteesta tai puhelinnumerosta? Tässä Elixir-ohjelmoinnin artikkelissa kerromme, miten voit helposti tehdä tämän käyttämällä substr() -funktiota. 

## Miten 
```Elixir
s = "Tämä on merkkijono"
```
GIVEN -funktio antaa meille merkkijonon, jota haluamme käsitellä. 

```Elixir
substring = String.substring(s, 0, 5)
IO.puts substring
```
Nämä kaksi riviä koodia käyttävät substring() funktiota, joka ottaa parametreinä merkkijonon, aloitusindeksin ja lopetusindeksin. Tässä tapauksessa se ottaa 0:n ensimmäisenä ja 5: n viidentenä, mikä tarkoittaa ensimmäisen viiden merkin saamista sana "Tämä". 

```
Tämä
```

Ja nyt meillä on vain haluamamme substrit.

## Syvempi sukellus
Kun käytät substr() -funktiota, on tärkeää huomata, että indeksit alkavat nollasta ja että lopetusindeksi ei sisälly osaan. Esimerkiksi merkkijonolla "Hello" aloitusindeksi on 0, ensimmäinen sana on H ja lopetusindeksi on 4, joten tulos on "Hell". 

Toinen tärkeä asia on, että substr() -funktion palauttama merkkijono on uusi kopio alkuperäisestä merkkijonosta. Tämä tarkoittaa, että alkuperäinen merkkijono ei muutu substrin ottamisen jälkeen. 

## Katso myös
- [String.substring/3 virallinen dokumentaatio](https://hexdocs.pm/elixir/String.html#substring/3)
- [Elixir-nimien ja funktioiden käytäntö](https://hexdocs.pm/elixir/naming-conventions.html)