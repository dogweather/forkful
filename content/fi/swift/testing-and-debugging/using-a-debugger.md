---
date: 2024-01-26 04:10:45.257896-07:00
description: "Xcode-debuggerin (Swiftin IDE) k\xE4ytt\xE4miseen voit asettaa keskeytyskohtia,\
  \ tarkastaa muuttujia ja seurata lausekkeita. T\xE4ss\xE4 on esimerkki: ```Swift\
  \ func\u2026"
lastmod: '2024-03-13T22:44:56.911375-06:00'
model: gpt-4-0125-preview
summary: "Xcode-debuggerin (Swiftin IDE) k\xE4ytt\xE4miseen voit asettaa keskeytyskohtia,\
  \ tarkastaa muuttujia ja seurata lausekkeita."
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:
Xcode-debuggerin (Swiftin IDE) käyttämiseen voit asettaa keskeytyskohtia, tarkastaa muuttujia ja seurata lausekkeita. Tässä on esimerkki:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Aseta keskeytyskohta napsauttamalla rivinumeroa Xcodessa vasemmalla ja suorita ohjelma. Kun se osuu keskeytyskohtaan, Xcode keskeyttää suorituksen. Nyt voit:

1. Tarkistaa muuttujien arvot.
2. Siirtyä yli (suorita seuraava rivi) tai siirtyä sisään (mene funktion sisään) käyttäen debuggerin hallintalaitteita.
3. Lisätä lausekkeita 'seurantalistaan' tarkkaillaksesi tiettyjen muuttujien tai vakioarvojen muutoksia.

Tässä mitä saatat nähdä debug-alueella:

```
(lldb) po number
5
(lldb) po result
120
```

## Syväsukellus:
Debuggerit ovat olleet osa ohjelmointimaisemaa 1940-luvulta lähtien, kehittyen yksinkertaisista keskeytysjärjestelmistä monimutkaisiin, käyttöliittymävetoisiin kokemuksiin. Xcoden sisäänrakennettua debuggeria lukuun ottamatta muita vaihtoehtoja sisältävät kolmannen osapuolen työkalut, kuten LLDB (Low Level Debugger), jota Xcode käyttää konepellin alla. Jotkut jopa debuggaavat `print()`-lauseilla (leikkisästi kutsuttu "luolamies-debuggaukseksi"), mutta tämä on vähemmän tehokas suurille projekteille tai monimutkaisille bugeille. Kun käytät debuggeria, jonglööraat suorituksen hallinnan, suoritusaikaisen introspektioinnin ja datamanipulaation kanssa. Syvällinen näiden periaatteiden ymmärtäminen on pitkällä matkalla tehokkaaseen debuggaukseen.

## Katso myös:
- [Applen Xcode Debugging -opas](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB Pikakäynnistysopas](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlichin Swift Debugging -opas](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
