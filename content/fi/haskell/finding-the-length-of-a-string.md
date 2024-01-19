---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Haskell-ohjelmointi: Merkkijonon pituuden määrittäminen

## Mikä & Miksi?
Merkkijonon pituuden määrittämisellä tarkoitetaan yksinkertaisesti sen mittaamista, kuinka monta merkkiä merkkijonossa on. Tämä on oleellista esimerkiksi silloin, kun haluamme suodattaa tai jakaa tietoa merkkijonon pituuden perusteella.

## Kuinka:
Käsittelimme merkkijonon pituuden määrittämistä `length` funktion avulla, nähdään esimerkki:

```Haskell
let str = "Hei Suomi"
print (length str)
```  
Tulostuu: 9

## Syvempi tarkastelu
Haskell-ohjelmointikielen `length` funktio on peräisin ensimmäisestä julkaisusta vuonna 1990. Se palauttaa merkkijonon pituuden kokonaislukuna.

Vaihtoehtoinen tapa olisi käyttää `foldl`-funktiota merkkijonon pituuden laskemiseen:

```Haskell
let str = "Hei Suomi"
let len = foldl (\n _ -> n + 1) 0 str 
print len
```  
Tulostuu: 9

Tämä tapa on monimutkaisempi eikä sitä yleensä suositella, kun yksinkertainen `length`-funktio on käytettävissä.

## Katso myös
Lue lisää Haskellin merkkijonoista ja funktioista:

1. "Learn You a Haskell for Great Good" - [Merkit ja merkkijonot](http://learnyouahaskell.com/starting-out#strings)
2. Hoogle - [length](https://hoogle.haskell.org/?hoogle=length) 
3. Haskell Wiki - [Haskell merkkijonot](https://wiki.haskell.org/Strings)