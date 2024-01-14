---
title:                "Bash: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat käyttävät satunnaislukuja monissa ohjelmissaan. Satunnaislukuja voidaan käyttää esimerkiksi salasanoiden luomiseen, arpajaisiin tai tietokoneen pelien kehittämiseen. Satunnaislukujen käyttö antaa ohjelmille ennakoimattomuutta ja tekee niistä mielenkiintoisempia ja monipuolisempia.

## Miten

Satunnaislukuja voidaan generoida helposti ja nopeasti Bash-skripteillä. Voit käyttää `$RANDOM`-muuttujaa, joka sisältää satunnaisen numeron välillä 0-32767. Tämän avulla voit esimerkiksi generoida satunnaisen kokonaisluvun seuraavasti:

```Bash
number=$((RANDOM % 100)) 
echo $number
```
Tämä koodi generoi satunnaisen numeron väliltä 0-99 ja tulostaa sen.

Voit myös käyttää `shuf`-komennolla satunnaisia merkkijonoja. Esimerkiksi seuraava koodi generoi satunnaisen salasanan, joka koostuu 10 merkistä:

```Bash
shuf -n 10 -e {a..z} {A..Z} {0..9} | tr -d '\n'
```

Voit myös asettaa satunnaisen siemenen käyttämällä `RANDOM`-muuttujaa yhdessä `RANDOMSEED`-muuttujan kanssa. Tämä mahdollistaa saman satunnaisen luvun generoimisen eri kertaa ajettuna. Esimerkiksi seuraava koodi generoi 10 erilaista satunnaislukua väliltä 1-100:

```Bash
for ((i=0;i<10;i++)); do
    seed=$(($RANDOM % 100))
    echo "Seed: $seed"
    RANDOMSEED=$seed
    echo "Random number: $((RANDOM % 100))"
    echo
done 
```

## Syväri

Satunnaislukujen generoiminen Bashissa perustuu POSIX-Standardin `rand()`-funktioon. Tämän funktion avulla generoidaan satunnaisia lukuja `RANDOM`-muuttujan avulla. `RANDOMSEED`-muuttuja puolestaan määrittää satunnaisen siemenen `srand()`-funktiolle, joka alustaa satunnaislukugeneroinnin. Tämä mahdollistaa sen, että voit käyttää Bashissa samanlaista satunnaislukugenerointia kuin C-kielellä.

## Katso myös
- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Freedesktop-kirjasto](https://www.freedesktop.org/software/bash/)
- [The Geek Stuff -kirjoitus satunnaislukujen generoinnista Bashissa](https://www.geeksforgeeks.org/bash-shell-using-random/)