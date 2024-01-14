---
title:                "Elm: Tekstitiedoston lukeminen"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedosto?

Tekstitiedostot ovat yleinen tapa tallentaa ja jakaa tietoa, joten on tärkeää osata lukea niitä. Elm-ohjelmointikielellä tekstien lukeminen on helppoa ja tehokasta. Seuraavassa opas siitä, miten se tapahtuu.

## Miten lukea tekstitiedosto Elmillä?

```Elm
import File exposing (readTextFile)
import Task exposing (attempt)

-- Avataan tiedosto ja luetaan sen sisältö
result = Task.attempt readFile (File.readTextFile "tekstitiedosto.txt")

-- Tulostetaan tiedoston sisältö konsoliin
readFile result = 
    case result of 
        Ok content -> 
            Debug.log "Tekstin sisältö:" content
        Err error -> 
            Debug.log "Virhe:" error
```

Tässä esimerkissä ensin tuodaan File-kirjasto, jolla voidaan lukea tekstitiedostoja. Sitten tehdään tehtävä result, joka kutsuu readFile-funktiota, joka lukee tiedoston ja palauttaa sen sisällön. Lopuksi content tai mahdollinen virhe tulostetaan konsoliin.

## Syvemmällä tekstitiedoston lukemisessa

Voit myös käsitellä tekstitiedoston sisältöä enemmän. Esimerkiksi voit käyttää String-moduulia etsimään tietoa tai käyttää Json-dekoodausta jos tiedostossa on JSON-muotoista dataa. Mahdollisuudet ovat rajattomat!

## Katso myös
- [Elm File -kirjasto](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Elm String -moduuli](https://package.elm-lang.org/packages/elm/core/latest/String)
- [JSON-dekoodaus Elmillä](https://guide.elm-lang.org/error_handling/json.html)