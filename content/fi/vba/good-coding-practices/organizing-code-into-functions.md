---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:48.087391-07:00
description: "Kuinka: VBA:ssa funktiot m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4m\xE4\
  ll\xE4 `Function` ja `End Function` -lauseita. T\xE4ss\xE4 on yksinkertainen esimerkki\
  \ siit\xE4, miten luoda funktio,\u2026"
lastmod: '2024-03-13T22:44:56.406766-06:00'
model: gpt-4-0125-preview
summary: "VBA:ssa funktiot m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4m\xE4ll\xE4 `Function`\
  \ ja `End Function` -lauseita."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:
VBA:ssa funktiot määritellään käyttämällä `Function` ja `End Function` -lauseita. Tässä on yksinkertainen esimerkki siitä, miten luoda funktio, joka laskee suorakulmion pinta-alan:

```basic
Function CalculateArea(length As Double, width As Double) As Double
    CalculateArea = length * width
End Function
```

Tämän funktion kutsumiseksi VBA-koodissasi ja tuloksen näyttämiseksi viestiruudussa käyttäisit:

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "Pinta-ala on " & area
End Sub
```

Kun tämä koodi suoritetaan, se näyttää viestiruudun, jossa sanotaan: `Pinta-ala on 50`.

### Muuttujien välittäminen ByRef ja ByVal avulla
VBA mahdollistaa muuttujien välittämisen funktioihin joko viittauksella (`ByRef`) tai arvolla (`ByVal`). Ensimmäinen tarkoittaa, että alkuperäistä muuttujaa voidaan muokata funktiossa, kun taas jälkimmäinen välittää kopion, suojellen alkuperäistä muuttujaa muutoksilta.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Syväsukellus
VBA, tapahtumavetoisena ohjelmointikielenä, korostaa merkittävästi funktioiden ja aliohjelmien käyttöä erilaisten tehtävien käsittelyyn. Toisin kuin monet nykyaikaiset kielet, VBAlla on ainutlaatuinen piirre, jossa `Function`-avainsana ei ainoastaan julista uudelleenkäytettävää koodilohkoa, vaan myös mahdollistaa implisiittisen palautusarvon suoran sijoittamisen funktion nimeen.

Historiallisesti VBA-funktioiden suunnitteluun on vaikuttanut aikaisemmat ohjelmointiparadigmat, joissa kapselointi ja modulaarisuus tunnustettiin vähitellen tärkeiksi ohjelmistokehityksessä. Tämä historiallinen tausta on johtanut VBA:n suht konservatiiviseen, mutta toiminnalliseen lähestymistapaan koodin järjestämisessä.

Vaikka VBA on tehokas omassa ympäristössään (esimerkiksi Microsoft Office -sovelluksissa), on tärkeää huomata, että ohjelmointimaailma on kehittynyt. Kielet, kuten Python, tarjoavat yksinkertaisemman syntaksin ja laajan vakio-kirjaston, mikä tekee niistä suosittuja vaihtoehtoja erilaisiin sovelluksiin Officen ulkopuolella. Kuitenkin työskenneltäessä Microsoft Office -tuotteiden kanssa, VBA:n integrointi- ja automaatiovalmiudet ovat vertaansa vailla.

On syytä huomata, että huolimatta iästään, VBA-yhteisö pysyy aktiivisena, löytäen jatkuvasti innovatiivisia tapoja hyödyntää sen toiminnallisuuksia. Kuitenkin, kun ohjelmistoteollisuus siirtyy kohti modernimpia, monipuolisempia ja luotettavampia kieliä, ohjelmoijia, jotka ovat perehtyneet VBA:han, kannustetaan tutkimaan näitä vaihtoehtoja Officeen liittymättömissä tehtävissä laajentaakseen koodaustyökalupakkiaan.
