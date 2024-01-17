---
title:                "Merkkijonon interpolointi"
html_title:           "Elixir: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Merkittävä osa ohjelmoinnista on tekstin käsittely ja manipulointi. Stringien interpoloiminen on yksi tapa tehdä tekstinkäsittelystä helpompaa ja tehokkaampaa. Interpolointi tarkoittaa muuttujien, arvojen tai ilmaisujen lisäämistä tekstin sekaan. Ohjelmoijien käyttäessä tätä tekniikkaa, he voivat helposti yhdistää dynaamista dataa ja kiinteitä tekstejä luomalla lopulta yhdistettyä dataa. Tämä on erityisen hyödyllistä tietojen visualisoinnissa ja raportoinnissa.

## Miten tehdä?

Elixirissä stringien interpolointi tapahtuu lähettämällä merkkijono ```#{arvo}``` -muodossa käytetty muuttujan nimi. Aina kun Elixir tapaa tämän rakenteen, se korvaa sen muuttujan arvolla. Tämä on yksinkertainen esimerkki interpoloinnista:

```Elixir
nimi = "Kati"
"Iltaa, #{nimi}"
```

Tämä tuottaa seuraavan tulosteen:

```Elixir
"Iltaa, Kati"
```

Voit myös käyttää interpolointia aritmeettisiin laskutoimituksiin. Esimerkiksi:

```Elixir
"Iästäsi 10 vuotta plussaat #{age + 10}"
```

Tämä tulostaa "Iästäsi 10 vuotta plussaat 20", jos age-muuttuja on arvoltaan 10.

## Syvempi sukellus

Interpolointi ei ole vain Elixirin ominaisuus, se on laajalti käytetty tekniikka erilaisissa ohjelmointikielissä. Joissakin kielissä interpolointi tapahtuu käyttämällä %-merkkiä. Esimerkiksi Rubyssa stringit interpoloidaan seuraavasti: ```"Iltaa, %s" % nimi```. Tämäntyyppinen interpolointi on myös mahdollista Elixirissä, mutta sen suositteleminen ei ole suositeltavaa, koska se ei ole yhtä tehokas kuin yllä kuvattu tapa.

Elixir käyttää `__interpolate__`-funktiota stringien manipuloinnissa. Kun interpolointi otetaan käyttöön, Elixir luo uuden version merkkijonosta, joka sisältää merkinnät muuttujien sijoittamisesta. Tämä tekee interpoloinnista turvallisen toimenpiteen, sillä muuttujien arvot muunnetaan automaattisesti merkkijonoiksi, estäen injektiohyökkäykset.

## Katso myös

Lisätietoa interpoloinnista ja muista tapauksista, joissa se voi olla hyödyllinen, löydät [Elixirin viralliselta sivustolta](https://elixir-lang.org/getting-started/string-interpolation.html). Kannattaa myös tutustua [interpoloinnin erotuksiin eri ohjelmointikielillä](https://stackoverflow.com/questions/2546097/what-is-string-interpolation) ja siihen, [miten turvallista interpolointia voidaan tehdä muissa kielissä](https://mixedmath.wordpress.com/2012/10/10/string-interpolation-in-python-r-elisp-c-c-c-java-erlang-javascript-ruby-and-the-kitchen-sink/).