---
title:    "Haskell: Virheilmoitustulostuksen tekeminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmistokehittäjät ovat varmasti kuulleet käsitteen "debuggaus" ja sen tärkeyden. Debuggaus tarkoittaa virheiden etsimistä ja korjaamista ohjelmointiprosessin aikana. Yksi tapa helpottaa tätä prosessia on käyttää debug tulostusta, joka tulostaa tietoa ohjelman suorituksen aikana. Tässä blogikirjoituksessa käymme läpi, miten voit lisätä debug tulostusta Haskell koodiisi ja miten se voi auttaa sinua kehittäjänä.

## Miten

Debug tulostuksen lisääminen Haskell koodiin ei ole vaikeaa. Voit käyttää "Debug.Trace" moduulia, joka tarjoaa valmiit funktiot debug tulostuksen tekemiseen.

```Haskell
import Debug.Trace

-- Yksinkertainen funktio, joka laskee kahden luvun summasta kolmatta lukua
laskeSumma :: Int -> Int -> Int
laskeSumma x y = x + y + trace ("Lasketaan summa " ++ show x ++ " ja " ++ show y) 0

-- Kutsutaan funktiota ja tulostetaan debug viesti
main = do
  let result = laskeSumma 5 2 -- 7
  print result
```

Tässä esimerkissä käytimme "trace" funktiota, joka ottaa parametrikseen merkkijonon ja tulostaa sen sekä palauttaa sen jälkeen funktion arvon. Näin voimme nähdä, mitä funktiossa tapahtuu suorituksen aikana. Huomaathan, että "trace" funktiota tulisi käyttää vain debug tarkoituksiin ja se kannattaa poistaa ennen kuin koodi siirtyy tuotantokäyttöön.

## Syväsukellus

Debug tulostuksen lisääminen voi auttaa sinua ymmärtämään paremmin, mitä ohjelmasi tekee suorituksen aikana. Voit esimerkiksi tulostaa muuttujien arvoja ja tarkistaa, että ne ovat odotetut. Tämä voi auttaa havaitsemaan mahdollisia virheitä ja nopeuttamaan debuggausprosessia.

On kuitenkin tärkeää olla varovainen debug tulostuksen käytössä, jotta siitä ei tule turhaa koodin sekaannusta ja sen suoritus hidastu. Voit esimerkiksi luoda "debug" funktioita, jotka ottavat parametrikseen toisen funktion ja suorittavat sen vain, jos debug tila on päällä. Näin vältät tarpeettomien debug tulostusten tekemisen.

## Katso myös

- [Debug.Trace dokumentaatio](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
- [Haskell Debugging - Your Guide to Debugging Haskell Code](https://www.fpcomplete.com/blog/haskell-debugging-guide/)
- [Debugging in Haskell](https://wiki.haskell.org/Debugging)
- [Tracing with Debug.Trace](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/14-Tracing-with-Debug-Trace)