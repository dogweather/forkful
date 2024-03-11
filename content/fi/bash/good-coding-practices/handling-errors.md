---
date: 2024-01-26 00:49:50.338056-07:00
description: "Virheiden k\xE4sittely Bash-skriptauksessa tarkoittaa mahdollisten ongelmakohtien\
  \ ennakointia ja niiden arvokasta k\xE4sittely\xE4. Miksi? No, se tekee\u2026"
lastmod: '2024-03-11T00:14:30.702896-06:00'
model: gpt-4-1106-preview
summary: "Virheiden k\xE4sittely Bash-skriptauksessa tarkoittaa mahdollisten ongelmakohtien\
  \ ennakointia ja niiden arvokasta k\xE4sittely\xE4. Miksi? No, se tekee\u2026"
title: "Virheiden k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden käsittely Bash-skriptauksessa tarkoittaa mahdollisten ongelmakohtien ennakointia ja niiden arvokasta käsittelyä. Miksi? No, se tekee skriptistäsi luotettavan ja säästää käyttäjiä päänraapimiselta, kun asiat eivät toimi odotetulla tavalla.

## Kuinka:

```Bash
#!/bin/bash

# Ohjaa stderr tiedostoon
grep "jotakin" tiedosto.txt 2> virheet.log

# Virheenkäsittely poistumisstatuksilla
if ! grep "jotakin" tiedosto.txt; then
    echo "Hups, jotakin meni pieleen 'jotakin' etsiessä."
    exit 1
fi

# Käyttäen loukkua virhetilanteessa siivotaan ennen poistumista
cleanup() {
  echo "Siivotaan väliaikaiset tiedostot..."
  rm temp_*
}

trap cleanup ERR

# tahallinen virhe: tiedostoa ei ole olemassa
cat temp_tiedosto.txt
```

Esimerkkituloste kun virhe tapahtuu:

```
Siivotaan väliaikaiset tiedostot...
cat: temp_tiedosto.txt: Tiedostoa tai hakemistoa ei ole
```

## Syväluotaus

Virheenkäsittely Bash-skriptauksessa juontaa juurensa Unix-komentotulkin alkuaikoihin, jolloin luotettavat ja kestävät skriptit olivat (ja ovat) elintärkeitä järjestelmien hallinnassa ja automaatioissa. Perinteisesti Bashissa virheet käsitellään tarkistamalla komennon poistumisstatus, joka konventioiden mukaan palauttaa 0 onnistumisesta ja muun kuin nollan epäonnistumisesta.

Bash esitteli `trap`-komennon sisäänrakennettuna, joka sallii käyttäjien määritellä komentoja suoritettavaksi erilaisissa signaaleissa tai skriptin poistumisen yhteydessä. Tämä on hyödyllistä esimerkiksi siivoustoimiin tai viimeiseksi keinoksi virheenkäsittelymekanismina.

On myös `set`-komento, jolla voidaan muuttaa Bashin toimintaa virhetilanteissa. Esimerkiksi `set -e` saa skriptin poistumaan välittömästi, jos mikä tahansa komento poistuu muulla kuin nollastatuksella, keino epäonnistua nopeasti ja välttää laajenevat virheet.

Vaihtoehtoja Bashin sisäiselle virheenkäsittelylle sisältävät mm. tiedostojen olemassaolon eksplisiittisen tarkistamisen, komentosubstituution käytön, tai jopa oman virheidenkäsittelyfunktioiden kirjoittamisen enemmän yksityiskohtiin menevälle käsittelylle.

Vaikka huolellinen virheenkäsittely voi joskus tuntua ylilyönniltä pienille skripteille, se on harjoitus, joka voi säästää paljon aikaa debuggauksessa ja estää odottamattomia käyttäytymisiä sekä sinulle että käyttäjille.

## Katso Myös

- Bash-käsikirja Kuoriparametreista: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Advanced Bash-Scripting Guide -osio Virheenkäsittelystä: https://www.tldp.org/LDP/abs/html/exit-status.html
- Syvällinen opas `trap`:iin: https://mywiki.wooledge.org/SignalTrap

Muista, skriptaus on taiteenlaji, ja se, miten käsittelet lipsahduksia ja kompastuksia, voi tehdä mestariteoksestasi kestävämmän. Onnellista skriptailua!
