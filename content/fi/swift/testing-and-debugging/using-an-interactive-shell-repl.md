---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
aliases:
- /fi/swift/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:08.445119-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Käyttämällä interaktiivista kuorta eli Read-Eval-Print-Loopia (REPL) voit koodata vuorovaikutteisesti. Ohjelmoijat käyttävät sitä Swift-koodinpätkien nopeaan testaamiseen, vianetsintään tai kielen opiskeluun.

## Kuinka:
Kutsu REPL käyttöön avaamalla terminaali ja suorittamalla komento `swift`. Kirjoita koodia suoraan ja paina Enter suorittaaksesi sen. Tässä maistiainen:

```Swift
1> let tervehdys = "Hei, REPL!"
tervehdys: String = "Hei, REPL!"
2> print(tervehdys)
Hei, REPL!
```

Poistu komennolla `:quit` tai `Control-D`.

## Syväsukellus
REPL:n juuret ulottuvat kauas, aina 60-luvun Lisp-tulkeihin. Swiftin REPL perustuu LLVM:ään, tehokkaaseen kääntäjäkehykseen, tarjoten enemmän kuin pelkän perustulkinnan – se on täysiverinen työkalu, jossa on täydennys, vianetsintä ja paljon muuta. REPL on loistava oppimiseen tai prototyyppien tekemiseen, mutta se ei ole itsenäinen kehitysympäristö. Jotkut suosivat Xcoden Playgroundsia graafisempiin, tiedostopohjaisiin projekteihin, kun taas toiset pysyvät perinteisessä skriptien muokkaamisessa ja suorittamisessa.

Pinnan alla Swiftin REPL dynaamisesti kääntää koodin konekielelle ja suorittaa sen, mikä tekee siitä suhteellisen nopean. Se voi myös käyttää mitä tahansa käännetyitä Swift-moduuleja tai jopa C-kirjastoja, tehden siitä varsin tehokkaan. Huomaa kuitenkin, että kaikki ei toimi täydellisesti REPL:ssä; jotkut Swiftin ominaisuudet, erityisesti ne, jotka vaativat monimutkaisia projektiasetuksia tai tarinakarttatiedostoja, eivät onnistu täällä.

## Katso Myös
- [Swift.org - Aloittaminen](https://www.swift.org/getting-started/#using-the-repl)
- Applen [Johdatus Xcoden Playgroundseihin](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM Project](https://llvm.org/)
