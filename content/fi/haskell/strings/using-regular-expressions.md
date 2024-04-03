---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:58.946175-07:00
description: "Kuinka: Haskellissa s\xE4\xE4nn\xF6llisen lausekkeen toiminnot eiv\xE4\
  t kuulu vakio kirjastoon, joten on tarpeen k\xE4ytt\xE4\xE4 kolmannen osapuolen\
  \ paketteja kuten `regex-\u2026"
lastmod: '2024-03-13T22:44:56.605781-06:00'
model: gpt-4-0125-preview
summary: "Haskellissa s\xE4\xE4nn\xF6llisen lausekkeen toiminnot eiv\xE4t kuulu vakio\
  \ kirjastoon, joten on tarpeen k\xE4ytt\xE4\xE4 kolmannen osapuolen paketteja kuten\
  \ `regex-base` yhdess\xE4 yhteensopivan taustaj\xE4rjestelm\xE4n kanssa kuten `regex-posix`\
  \ (POSIX-yhteensopivaa regex-tukea varten), `regex-pcre` (Perl-yhteensopivaa regex-tukea\
  \ varten), jne."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Kuinka:
Haskellissa säännöllisen lausekkeen toiminnot eivät kuulu vakio kirjastoon, joten on tarpeen käyttää kolmannen osapuolen paketteja kuten `regex-base` yhdessä yhteensopivan taustajärjestelmän kanssa kuten `regex-posix` (POSIX-yhteensopivaa regex-tukea varten), `regex-pcre` (Perl-yhteensopivaa regex-tukea varten), jne. Tässä on, miten voit käyttää näitä paketteja työskennelläksesi säännöllisten lausekkeiden kanssa.

Ensin, varmista että sinulla on paketit asennettuna lisäämällä `regex-posix` tai `regex-pcre` projektisi `.cabal` tiedostoon tai asentamalla suoraan cabalin kautta:

```bash
cabal install regex-posix
```
tai
```bash
cabal install regex-pcre
```

### Käyttäen `regex-posix`:
```haskell
import Text.Regex.Posix ((=~))

-- Tarkista, vastaako merkkijono kaavaa
isMatch :: String -> String -> Bool
isMatch teksti kaava = teksti =~ kaava :: Bool

-- Etsi ensimmäinen vastaavuus
findFirst :: String -> String -> String
findFirst teksti kaava = teksti =~ kaava :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Tuloste: True
    print $ findFirst "good morning, good night" "good"
    -- Tuloste: "good"
```

### Käyttäen `regex-pcre`:
```haskell
import Text.Regex.PCRE ((=~))

-- Etsi kaikki vastaavuudet
findAll :: String -> String -> [String]
findAll teksti kaava = teksti =~ kaava :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Tuloste: ["test1","test2","test3"]
```

Kullakin kirjastolla on omat erityispiirteensä, mutta yleinen metodologia `=~` käytön kanssa regexin soveltamiseksi pysyy johdonmukaisena, oli kyseessä sitten vastaavuuden tarkistus tai alimerkkijonojen poiminta. Valinta `regex-posix` ja `regex-pcre` välillä riippuu suuresti projektisi tarpeista ja vaaditusta erityisestä regex-ominaisuudesta.
