---
title:                "Debuggerin käyttö"
aliases:
- /fi/python/using-a-debugger/
date:                  2024-01-26T04:09:00.655890-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
"Debuggerin käyttö" tarkoittaa Python-koodisi läpikäymistä askel askeleelta virheiden löytämiseksi ja toiminnan ymmärtämiseksi. Teemme niin, koska se on paljon helpompaa kuin vain arvailu, missä asiat menivät pieleen, ja se säästää meidät tuntikausilta tulostuslauseiden purgatoriosta.

## Kuinka:
Käydään läpi `pdb`:n, Pythonin sisäänrakennetun debuggerin, käyttö. Kuvittele tiedosto, `buggy.py`, jossa on ovela bugi:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Kun ajat tämän skriptin, odotat saavasi `8`, mutta saat vain syntaksivirheen. On debuggerin aika!

Terminaalissasi, suorita:
```bash
python -m pdb buggy.py
```

Pääset debuggeriin, ja se näyttää tältä:
```Python
> /polku_tiedostoon/buggy.py(1)<module>()
-> def add_one(number):
```

Käytä `l(ist)` nähdäksesi lisää koodia, `n(ext)` siirtyäksesi seuraavalle riville tai `c(ontinue)` jatkaaksesi skriptin suorittamista. Kun kohtaat virheen, `pdb` pysähtyy ja antaa sinulle mahdollisuuden tutkia.

Korjattuasi `number ++ 1` muotoon `number + 1`, käynnistä debuggeri uudelleen korjauksen testaamiseksi.
Muista, kaverit eivät anna kavereidensa koodata ilman verkkoa. Siinä kaikki.

## Syvä sukellus
Ohjelmoinnin pimeinä aikoina (eli ennen kuin integroidut kehitysympäristöt, eli IDE:t, olivat kaikkialla), debuggerit olivat usein itsenäisiä työkaluja, joita käytettäisiin tekstimuokkaimen ulkopuolella. Ne tulivat apuun antamalla ohjelmoijien tarkastella ohjelmistonsa tilaa eri suorituspisteissä.

Vuonna 2023 Pythonin `pdb` ei ole ainoa vaihtoehto. Ihmiset saattavat käyttää IDE:iä kuten PyCharm tai Visual Studio Code, joissa on omat tyylikkäät debuggerinsa sisäänrakennettuina. Nämä tarjoavat käteviä ominaisuuksia, kuten klikkauksella asetettavat keskeytyskohdat, eikä kryptisten komentojen kirjoittamista tarvita.

Sitten on `ipdb`, pip:llä asennettava paketti, joka tuo `IPython`-herkut debuggaukseen. Se on kuin `pdb` suorituskykyä parantavilla aineilla, tarjoten välilehden täydennyksen ja syntaksikorostuksen.

Debuggerit vaihtelevat myös toteutuksessaan. Jotkut pääsevät lähelle ohjelman suoritusta kone- tai tavukooditasolla. Toiset, kuten monet korkean tason kielen debuggerit, suorittavat koodin erityisessä ympäristössä, joka tarkkailee muuttujien tilaa ja ohjaa suoritusvirtaa.

## Katso myös
Lisätietoja Pythonin omasta debuggerista löydät:
- `pdb` dokumentaatio: https://docs.python.org/3/library/pdb.html

Jos olet utelias vaihtoehdoista, nämä linkit palvelevat sinua hyvin:
- `ipdb` repo ja käyttöopas: https://github.com/gotcha/ipdb
- Debuggaus Visual Studio Codella: https://code.visualstudio.com/docs/python/debugging
- PyCharmin debuggausominaisuudet: https://www.jetbrains.com/help/pycharm/debugging-code.html

Onnea virheenmetsästykseen!
