---
title:    "Clojure: Merkkijonojen yhdistäminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin kielissä on mahdollista yhdistää erillisiä merkkijonoja yhdeksi isoksi merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun halutaan muodostaa dynaamisia viestejä tai kirjoittaa tietoja tiedostoon. Clojurella tämä onnistuu helposti käyttämällä `str` -funktiota.

## Miten

Yhdistäminen tapahtuu antamalla `str` -funktiolle halutut merkkijonot parametreina. Alla on esimerkki, jossa yhdistetään kaksi sanaa "Hei" ja "Maailma".

```Clojure
(str "Hei" "Maailma")
```

Tämän tuloksena saadaan merkkijono "HeiMaailma".

Toinen tapa yhdistää merkkijonoja on käyttää `str` -funktiota ja lisätä siihen väliä tai muita merkkejä parametreina. Esimerkki alle on listattu, kuinka voit lisätä nämä välimerkit `str` -funktiossa.

```Clojure
(str "Tervetuloa" " " "Suomeen" " " "!")
```

Tämä tuottaa tuloksena "Tervetuloa Suomeen !".

## Syvemmälle

`str` -funktio pystyy käsittelemään monia eri tyyppisiä arvoja, joten sen käyttömahdollisuudet ovat lähes rajattomat. Lisäksi tämä funktio on suorituskykyinen ja toimii hyvin myös suurilla merkkijonoilla.

On myös mahdollista käyttää `str` -funktiota luku- ja boolean -arvojen yhdistämiseen. Jos luku- tai boolean -arvo on mukana yhdistettävässä merkkijonossa, se muunnetaan automaattisesti merkkijonoksi.

Tässä on esimerkki luku- ja boolean -arvojen yhdistämisestä `str` -funktiolla.

```Clojure
(str "Tulosta arvoa" 5 "ja" true)
```

Tämä tuottaa tuloksen "Tulosta arvoa 5 ja true".

## Katso myös

- [Clojure dokumentaatio: str](https://clojure.org/api/string)
- [Clojure Cookbook: String Concatenation](https://clojure-cookbook.clojureverse.org/string_manipulation/string_concatenation.html)
- [Miten yhdistää merkkijonoja Clojuressa?](https://paulspontifications.blogspot.com/2011/03/how-to-join-strings-in-clojure.html)