---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:45.803491-07:00
description: "Testien kirjoittaminen Fish Shelliss\xE4 tarkoittaa skriptien luomista,\
  \ jotka automaattisesti suorittavat koodisi varmistaakseen sen toimivan odotetulla\u2026"
lastmod: '2024-03-13T22:44:56.998175-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Fish Shelliss\xE4 tarkoittaa skriptien luomista,\
  \ jotka automaattisesti suorittavat koodisi varmistaakseen sen toimivan odotetulla\
  \ tavalla."
title: Testien kirjoittaminen
weight: 36
---

## Mikä ja miksi?

Testien kirjoittaminen Fish Shellissä tarkoittaa skriptien luomista, jotka automaattisesti suorittavat koodisi varmistaakseen sen toimivan odotetulla tavalla. Tämä käytäntö on tärkeä, koska se varmistaa, että komentoskriptisi toimivat tarkoitetulla tavalla, havaitsee virheet aikaisin ja helpottaa ylläpitoa.

## Kuinka:

Fish ei sisällä sisäänrakennettua testauskehystä kuten jotkut muut ohjelmointiympäristöt. Voit kuitenkin kirjoittaa yksinkertaisia testiskriptejä, jotka käyttävät väittämiä funktioidesi käyttäytymisen tarkistamiseen. Lisäksi voit hyödyntää kolmannen osapuolen työkaluja, kuten `fishtape`, kattavampaan testauspakettiin.

### Esimerkki 1: Perustestiskripti

Aloitetaan perustoiminnolla Fishissä, joka laskee kahden luvun summan:

```fish
function add --description 'Lisää kaksi lukua'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Voit kirjoittaa tästä toiminnosta perustestiskriptin seuraavasti:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add passed"
    else
        echo "test_add failed"
    end
end

test_add
```

Tämän skriptin suorittaminen tuottaisi tuloksen:

```
test_add passed
```

### Esimerkki 2: Fishtape:n käyttö

Kattavampaan testiratkaisuun voit käyttää `fishtape`:a, joka on TAP-tuottava testiajuri Fishille.

Asenna ensin `fishtape`, jos et ole vielä tehnyt sitä:

```fish
fisher install jorgebucaran/fishtape
```

Luo seuraavaksi testitiedosto `add`-toiminnollesi, esim. `add_test.fish`:

```fish
test "Lisäämällä 3 ja 4 saadaan 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Suorittaaksesi testin, käytä seuraavaa komentoa:

```fish
fishtape add_test.fish
```

Näytekulostus saattaisi näyttää tältä:

```
TAP version 13
# Lisäämällä 3 ja 4 saadaan 7
ok 1 - test_add passed
```

Tämä kertoo, että testi läpäistiin onnistuneesti. `fishtape` mahdollistaa tarkemmin rakenteellisten testien luomisen ja tarjoaa informatiivisen tulosteen, mikä helpottaa virheenkorjausta ja kattavaa testikattavuutta Fish-skripteillesi.
