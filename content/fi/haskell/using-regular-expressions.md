---
title:    "Haskell: Säännöllisten lausekkeiden käyttö"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Regular expressionit (tavallisesti kutsutaan myös regexeiksi) ovat voimakas työkalu, joka auttaa sinua löytämään, muokkaamaan ja korvaamaan merkkijonoja ohjelmointitehtävissä. Niiden avulla voit vähentää koodin määrää ja lisätä sen luettavuutta. Tämä tekee regexeistä erittäin hyödyllisen työkalun monissa eri tilanteissa.

## Miten

Regular expressionit toimivat käyttäen säännöllisiä lausekkeita, jotka määrittelevät halutunlaisen merkkijonon. Käytettäessä sitä Haskellissa, on tärkeää tuoda import-komennolla "Text.Regex.Posix". Seuraavassa esimerkissä näytämme, miten voit käyttää regexejä löytääksesi kaikki sanan "tietokone" esiintymät annetusta merkkijonosta:

```Haskell 
import Text.Regex.Posix

myString :: String
myString = "Minulla on tietokone, joka on kolme vuotta vanha ja tarvitsee päivitystä."

computerRegex :: String
computerRegex = "\\btietokone\\b"

foundStrings :: [String]
foundStrings = getAllTextMatches (myString =~ computerRegex :: AllTextMatches [] String)

main :: IO ()
main = putStrLn $ show foundStrings

--[Output]
--["tietokone"]
```

Kuten näemme, käytimme regexiä, joka koostuu sanoista "\\\bcomputer\\\b". Tämä tarkoittaa, että etsimme sanaa "computer", joka on eristetty sanojen väliltä. Lopuksi käytämme getAllTextMatches-funktiota saadaksemme kaikki löytyneet merkkijonot ja tulostamme ne näytölle. Mukavaa ja yksinkertaista, eikö?

## Syväsukellus

Regular expressionit tarjoavat monia erilaisia kaavoja, jotka auttavat sinua löytämään tai muokkaamaan haluamasi merkkijonon. Esimerkiksi, "\\d" tarkoittaa, että etsit kaikkia numeromerkkejä ja "\\s" tarkoittaa, että etsit kaikkia tyhjiä välilyöntejä. Regexejä käytetään yleisesti myös tekstinkäsittelyohjelmissa, kuten Wordissa tai Excelissä, joten opitun regex-taidon voi ottaa mukaan myös muille alustoille.

## Katso myös

- [Haskell-ohjelmoinnin aloittaminen](https://haskell.fi/)
- [Virallinen Haskell-dokumentaatio regexeistä](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [FreeCodeCampin opas regexeihin](https://www.freecodecamp.org/news/a-quick-and-easy-guide-to-regular-expressions-regex/)