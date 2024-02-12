---
title:                "Debuggerin käyttö"
aliases:
- /fi/swift/using-a-debugger/
date:                  2024-01-26T04:10:45.257896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen tarkoittaa erikoistuneiden työkalujen hyödyntämistä koodisi testaamiseen ja tarkasteluun sen suorituksen aikana. Se on iso juttu, koska se antaa sinun nähdä, mitä konepellin alla tapahtuu, löytää bugeja ja ymmärtää paremmin koodisi käyttäytymistä.

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
