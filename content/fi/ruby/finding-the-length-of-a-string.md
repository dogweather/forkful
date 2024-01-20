---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pituuden määrittäminen tarkoittaa merkkijonoa muodostavien merkkien tai kirjainten kokonaismäärän laskemista. Sitä tarvitaan, kun ohjelmistot tarvitsevat tietää, kuinka paljon dataa merkkijonolla on, esimerkiksi validointiin, kaistaleveyden optimointiin tai tallennustilan arviointiin.

## Kuinka:

```Ruby
merkkijono = "Terve Suomi"
puts merkkijono.length
```

Esimerkin tulostus on `11`. `length`-metodi laskee merkkijonon merkkien määrän ja palauttaa sen kokonaislukuna.

```Ruby
tyhjä_merkkijono = ""
puts tyhjä_merkkijono.length
```

Tyhjän merkkijonon kohdalla, esimerkin tulostus on `0`.

## Syvällisemmin:

### Historia:

`length`- ja `size`-metodit ovat olleet osa Ruby-kieltä sen synnystä asti, sen perustajan Yukihiro "Matz" Matsumoton tavoitteena on ollut aina tehdä ohjelmoinnista yksinkertaista ja miellyttävää.

### Vaihtoehdot:

Rubyssa on myös `size`-metodi, joka toimii samalla tavalla kuin `length`:

```Ruby
merkkijono = "Terve Suomi"
puts merkkijono.size
```

Tässäkin tapauksessa tuotos on `11`.

### Yksityiskohdat:

`length`- ja `size`-metodit laskevat UTF-8-koodatut merkit tavuina, ei merkkeinä. Eripituisille merkeille, kuten emoji, se voi antaa odottamattoman arvon. Tässä tapauksessa tulisi käyttää `chars.count` tai `grapheme_clusters.count`.

## Katso Myös:

- Ruby String Documentation: https://ruby-doc.org/core/String.html
- How Does Ruby Handle UTF-8 Strings?: https://thoughtbot.com/blog/fight-back-utf-8-invalid-byte-sequences