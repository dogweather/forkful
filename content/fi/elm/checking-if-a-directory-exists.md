---
title:                "Elm: Tarkistetaan löytyykö kansio"
simple_title:         "Tarkistetaan löytyykö kansio"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kannattaa tarkistaa, onko hakemistoa olemassa? Tämä on tärkeää tietoa, jota tarvitaan ohjelmoinnissa esimerkiksi, kun halutaan varmistaa tiettyjen tiedostojen olemassaolo tietyssä hakemistossa ennen kuin niitä käytetään. Näin varmistetaan, että ohjelma toimii suunnitellusti ja vältetään mahdollisia virheitä.

## Miten tehdä se Elmilla

Tässä on esimerkki koodista, jolla voidaan tarkistaa, onko hakemistoa olemassa Elmilla:

```Elm
import File

directoryName = "hakemiston_nimi"

case File.exists directoryName of
    True -> "Hakemisto on olemassa."
    False -> "Hakemistoa ei löydy."
```

Koodin tulos riippuu siitä, onko annetulla nimellä olemassa oleva hakemisto vai ei. Jos hakemisto löytyy, tulostuu "Hakemisto on olemassa", jos taas ei, tulostuu "Hakemistoa ei löydy".

## Syvemmälle aiheeseen

Kun tarkistetaan hakemiston olemassaoloa, on tärkeää huomioida myös käsiteltävänä oleva käyttöjärjestelmä. Esimerkiksi Windows-käyttöjärjestelmässä hakemistot erotetaan kenoviivoilla ("\") kun taas Unix-pohjaisissa järjestelmissä käytetään käänteisillä vinoviivoilla ("/"). Tätä tulee ottaa huomioon, jotta koodi toimii kaikissa ympäristöissä oikein.

## Katso myös

- [Elm-tiedostojen käsittely](https://guide.elm-lang.org/interop/javascript.html)
- [Hakemistojen tarkistaminen JavaScriptillä](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Hakemistojen käsittely eri käyttöjärjestelmissä](https://www.learnshell.org/en/Handling-Files/Bash-Permissions/)