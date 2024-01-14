---
title:    "Elm: Komentoriviparametrien lukeminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Yksi tapa antaa käyttäjille mahdollisuus vaikuttaa ohjelman suoritukseen on käyttää komentoriviparametreja. Tässä blogikirjoituksessa opit, miten voit lukea ja käsitellä komentoriviparametreja Elm-ohjelmointikielessä.

## Kuinka tehdä

Seuraavassa esimerkissä näytämme, miten voit käyttää `CommandLine` -kirjastoa lukeaksesi komentoriviparametreja ja tulostamaan ne konsoliin. Voit kokeilla esimerkkiä suoraan Elm Replissä.

```elm
import CommandLine exposing (Args)

main : Program Never Never ()
main =
  CommandLine.read
    |> Task.map printArgs
    |> Task.perform (always ())

printArgs : Args -> ()
printArgs args =
  -- Tulostaa parametrit konsoliin
  List.map toString args.parameters |> Debug.log "Komentorivi parametrit"
```

### Tulostus

```
Komentorivi parametrit:
["--nimi" "Testikäyttäjä" "--paikka" "Helsinki"]
```

## Syvällisempi sukellus

`CommandLine` -kirjaston `Args` tyyppi tarjoaa pääsyn komentoriviparametreihin. Voit käyttää `Args` -tyypin funktioita, kuten `getValue` tai `hasFlag` saadaksesi haluamiasi tietoja parametreista.

Lisäksi, voit myös käyttää `Debug.log` funktiota tulostamaan parametreja konsoliin ja välittää niitä haluamillesi funktioille ohjelman suorituksen aikana.

## Katso myös

- [Elm CommandLine -kirjasto](https://package.elm-lang.org/packages/mgold/elm-args/latest/)
- [Ohjelmointikieli Elm suomeksi](https://www.koodikirja.fi/elm-suomenkielinen-opas/)