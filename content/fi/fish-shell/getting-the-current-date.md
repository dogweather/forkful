---
title:    "Fish Shell: Nykyisen päivämäärän hakeminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi
Koska Fish Shellilla on helppo ja nopea tapa saada nykyinen päivämäärä ja aika ulosohjelmasta.

## Miten
Fish Shellilla on sisäänrakennettu funktio, joka mahdollistaa nykyisen päivämäärän ja ajan saannin helposti. Käytä komentoa `date` ja määritä muoto, miten haluat päivämäärän tulostettavan. Alla on muutamia esimerkkejä koodista ja niiden tulostuksesta Fish Shellilla:

```fish
date +%x
```
Tulostaa nykyisen päivämäärän muodossa: 12/02/19.

```fish
date +%A, %B %d, %Y
```
Tulostaa nykyisen päivämäärän muodossa: Monday, December 02, 2019.

```fish
date +%r
```
Tulostaa nykyisen ajan muodossa: 10:30:25 PM.

## Syvällinen katsaus
Fish Shellin `date`-komento perustuu UNIX-käyttöjärjestelmässä käytettyyn `date`-komentoon. Tämä komento mahdollistaa monipuoliset muotoiluvaihtoehdot päivämäärän ja ajan tulostamiseen. Voit käyttää erilaisia %-merkkejä ja kirjaimia määrittääksesi, mitä tietoja haluat tulostettavan.

Voit myös käyttää `date`-ohjelmaa komentokehotteessa saadaksesi lisätietoa formaattisäännöistä. Voit esimerkiksi käyttää `man date` saadaksesi selitykset käytettävissä olevista muotoiluvaihtoehdoista.

## Katso myös
- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [UNIX-komento date](https://www.unix.com/man-page/osx/1/date/)
- [Muut hyödylliset Fish Shell komentot](https://github.com/jorgebucaran/awesome-fish)