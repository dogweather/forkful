---
title:                "Haskell: Merkkijonon pituuden löytäminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Stringin pituuden etsiminen voi olla hyödyllistä, kun haluat tarkistaa tekstin pituuden esimerkiksi lukeaksesi user inputtia tai tulostaa merkkijonon alueen.

## Miten
```Haskell
pituus:: String -> Int
pituus s = length s

pituus "Hei maailma" 
-- Palauttaa 11
```

Voit käyttää `length` funktiota saadaksesi merkkijonon pituuden. Tämä funktio ottaa argumentiksi merkkijonon ja palauttaa sen pituuden kokonaislukuna.

## Syvällisesti
Stringin pituus lasketaan siirtämällä jokainen merkki merkkijonossa yksi kerrallaan ja lisäämällä laskuri, joka pitää kirjaa merkkien määrästä. Tätä prosessia jatketaan kunnes merkkijono on käyty läpi ja laskuri on saavuttanut merkkijonon pituuden. Tämä tapahtuu rekursiivisesti eli funktio kutsuu itseään kunnes laskuri on valmis.

## Katso myös
- [Haskellin standardikirjaston dokumentaatio `length` funktiosta](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:length)
- [Artikkeli Haskellin rekursiivisesta laskennasta](https://en.wikibooks.org/wiki/Haskell/Recursion)
- [Lyhyt esimerkki Rekursiosta](https://www.haskell.org/tutorial/functions.html#functions-and-pattern-matching)