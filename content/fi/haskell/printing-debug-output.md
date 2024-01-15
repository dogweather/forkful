---
title:                "Virheentarkistuslohkojen tulostaminen"
html_title:           "Haskell: Virheentarkistuslohkojen tulostaminen"
simple_title:         "Virheentarkistuslohkojen tulostaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteiden tulostaminen on tärkeä osa koodauksen prosessia, jolla voit helposti löytää virheitä ja ongelmia koodissasi. Se säästää aikaa ja vaivaa, ja auttaa sinua kehittämään tehokkaampia ja virheettömiä ohjelmia.

## Kuinka

```Haskell
-- Esimerkki debug-tulosteen tulostamisesta
-- Kayttaen "print" funktiota
main = do
  print "Tama on debug-tuloste"
  print [1, 2, 3]

-- Vastaava tulos:
-- "Tama on debug-tuloste"
-- [1, 2, 3]
```

Voit myös käyttää "putStrLn" funktiota tulostamaan merkkijonoja ja "show" funktiota muuntamaan arvoja merkkijonoiksi.

```Haskell
-- Debug-tulosteen tulostaminen käyttäen "putStrLn" ja "show" funktioita
main = do
  putStrLn "Tama on debug-tuloste"
  putStrLn (show [1, 2, 3])

-- Vastaava tulos:
-- "Tama on debug-tuloste"
-- "[1, 2, 3]"
```

## Syvempi sukellus

Voit myös käyttää "trace" funktiota "Debug.Trace" moduulista tulostamaan debug-viestejä haluamassasi kohdassa koodia.

```Haskell
-- Debug-tulosteen tulostaminen käyttäen "trace" funktiota
import Debug.Trace

main = do
  let x = 10
  trace "X:n arvo:" (print x)

-- Vastaava tulos:
-- "X:n arvo:"
-- 10
```

Voit myös määrittää oman "trace" funktiosi, jossa voit käyttää haluamiasi merkkejä tai symboleja erottamaan viestejä ja arvoja.

```Haskell
-- Oma "trace" funktio, jossa käytetään "$" merkkiä erottamaan viesti ja arvo
trace' :: String -> a -> a
trace' msg val = trace (msg ++ ": $" ++ show val) val

main = do
  let x = 10
  trace' "X" x

-- Vastaava tulos:
-- "X: $10"
```

## Katso myös

- [Haskell Wikibooks: Debugging](https://en.wikibooks.org/wiki/Haskell/Debugging)
- [Debugging with Haskell's GHCi](http://brandon.si/code/debugging-with-haskells-ghci/)
- [Haskell Weekly: Debugging with Haskell](https://haskellweekly.news/issue/4.html)