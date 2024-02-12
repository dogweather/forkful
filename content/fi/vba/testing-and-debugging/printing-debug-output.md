---
title:                "Debug-tulosteen tulostaminen"
date:                  2024-02-01T21:58:38.326050-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-tulosteen tulostaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tulosteen tulostaminen debuggausta varten Visual Basic for Applications (VBA) -ohjelmassa sisältää tulostuslauseiden strategisen sijoittamisen koodiisi näyttämään muuttujien arvot, suorituksen kulun tai mukautetut debug-viestit. Tämä tekniikka on olennainen osa vianetsintää, sillä se mahdollistaa ohjelmoijien ymmärtää koodinsa käyttäytymistä suoritusaikana ja tunnistaa mahdolliset odottamattomat käyttäytymiset tai virheet.

## Miten:
VBA:ssa `Debug.Print`-lause on työjuhta debug-tietojen tulostamiseksi Välitön-ikkunaan Visual Basic Editorissa (VBE). Jotta voisit käyttää tätä ominaisuutta tehokkaasti, sinun täytyy pitää Välitön-ikkuna näkyvissä (Näkymä > Välitön ikkuna tai paina `Ctrl+G` VBE:ssä).

Tässä on yksinkertainen esimerkki `Debug.Print`-lauseen käyttämisestä muuttujan arvon ja mukautetun viestin tulostamiseen:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "Muuttujan sampleVar arvo on: "; sampleVar
End Sub
```

Kun ajat tämän aliohjelman, Välitön ikkuna näyttää:
```
Muuttujan sampleVar arvo on: 42
```

Voit myös käyttää sitä seurataksesi monimutkaisen ehdollisen logiikan kulkua lisäämällä `Debug.Print`-lauseita koodisi eri haaroissa:

```basic
Sub TarkistaArvo()
    Dim arvoTarkistettavana As Integer
    arvoTarkistettavana = 9
    
    If arvoTarkistettavana > 10 Then
        Debug.Print "Arvo on suurempi kuin 10."
    ElseIf arvoTarkistettavana < 10 And arvoTarkistettavana > 0 Then
        Debug.Print "Arvo on välillä 1 ja 9."
    Else
        Debug.Print "Arvo on 10 tai vähemmän kuin 1."
    End If
End Sub
```

`TarkistaArvo`:n suorittaminen tuottaa:
```
Arvo on välillä 1 ja 9.
```

Muista, että `Debug.Print`:in tuotos menee vain Välitön-ikkunaan, mikä on erittäin hyödyllistä kehitysvaiheen aikana, mutta ei näy yhtään missään käyttäjälle suunnatussa sovelluksen osassa.

## Syväsukellus
Välitön-ikkuna ja `Debug.Print`-metodi ovat syvällä Visual Basic for Applicationsin historiassa, heijastaen debuggauskäytäntöjen kehitystä ajan myötä. Alun perin vianetsintä oli tekstipohjaisempi ja vähemmän visuaalinen prosessi, jossa kehittäjät nojasivat vahvasti tulostuslauseisiin ymmärtääkseen mitä heidän koodinsa teki. Vuosien kuluessa, kun kehitysympäristöt kehittyivät, myös debuggaustyökalut kehittyivät, tuoden mukanaan katkaisupisteitä, tarkkailuja ja monimutkaisempia suorituskyvyn profilointityökaluja, jotka tarjoavat interaktiivisemman ja välittömämmän käsityksen koodin käyttäytymisestä.

Kuitenkin `Debug.Print` ja Välitön-ikkuna ovat yhä uskomattoman hyödyllisiä, erityisesti nopeisiin ja likaisiin debuggaussessioihin tai silloin kun käsitellään koodia, joka on hankala katkaista (kuten tapahtumankäsittelijät). Siitä huolimatta on tärkeää tunnistaa, että pelkästään tulostuslauseisiin nojaaminen debuggauksessa nykyaikaisessa ohjelmoinnissa voi olla vähemmän tehokasta verrattuna integroitujen debuggerien käyttöön, joissa on katkaisupiste-, tarkkailu- ja pinon tarkastusominaisuuksia.

Vaikka vaihtoehdot, kuten lokituskehykset tai kehittyneemmät debuggaustyökalut tarjoavat enemmän ominaisuuksia ja joustavuutta, `Debug.Print`:in yksinkertaisuus ja välitön hyödynnettävyys VBA:ssa tekevät siitä arvokkaan työkalun, erityisesti ohjelmoijille, jotka ovat siirtyneet muista kielistä ja ovat jo tottuneet tulostuspohjaisiin debuggaustekniikoihin. Kuitenkin, kun he tulevat mukavammiksi VBA:n ja Visual Basic Editorin kanssa, täyden debuggaustyökalujen valikoiman tutkiminen voi johtaa tehokkaampaan ja tehokkaampaan ongelmanratkaisuun.
