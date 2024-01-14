---
title:    "Elm: Tarkista onko hakemisto olemassa"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan blogipostausta Elm-ohjelmoinnista! Tämä viesti käsittelee hakemiston olemassaolon tarkistamista ja miten se tehdään Elm-kielellä.

Hakemistojen tarkistaminen on tärkeä osa ohjelmointia, koska se auttaa välttämään virheitä, kun ohjelma yrittää käsitellä tiedostoja, jotka eivät ole olemassa. Elmillä on hyvä tapa tarkistaa, onko hakemisto olemassa, ja tämän viestin avulla opit, miten se tehdään.

## Miten

Elm-kielessä hakemiston olemassaolon tarkistamiseen käytetään "Directory" -moduulia. Se tarjoaa "exists" -funktion, joka ottaa parametrinaan hakemiston polun ja palauttaa "Task" -tyypin arvon.

```
import Directory

Tämä esimerkki käyttää "exists" -funktiota tarkistamaan, onko hakemistossa "main" tiedostoa "Code.elm".
```

```Elm
Directory.exists "main/Code.elm"

-- Output: Ok True
```

Kuten voit nähdä, "exists" -funktio palauttaa "Ok" -arvon, jos hakemisto löytyy ja "False", jos sitä ei ole. Voit myös käyttää tämän funktion kanssa "Task" -tyypin funktioita, kuten "andThen" ja "map" saadaksesi tarkemman kontrollin ja käsittelyn hakemiston olemassaolosta riippuen.

## Syvemmälle

Tässä viestissä olemme vain pyörähtäneet pintaa hakemiston olemassaolon tarkistamisesta Elm-kielellä. Voit myös käyttää "Directory" -moduulia luomaan uusia hakemistoja, poistamaan niitä tai siirtämään tiedostoja hakemistojen välillä. Lisätietoja löydät Elm-kieleen liittyvistä oppimateriaaleista ja kirjoista.

## Katso myös

- [Elmin virallinen dokumentaatio hakemistojen käsittelystä](https://guide.elm-lang.org/interop/file_system.html)
- [Elm-kirjasto "elm-file-2"](https://package.elm-lang.org/packages/mpizenberg/elm-file/latest/)
- [Elm-ohjelmoinnin perusteet (suomeksi)](https://www.ohjelmointiputka.net/elm-kirja/)