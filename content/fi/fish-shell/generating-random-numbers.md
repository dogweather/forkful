---
title:                "Satunnaislukujen luominen"
html_title:           "Fish Shell: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi käyttää satunnaislukujen generointia?

Satunnaislukuja tarvitaan usein ohjelmoinnissa esimerkiksi testaamiseen tai satunnaisen käyttäjätoiminnan simuloimiseen. Satunnaislukujen generointi on myös hauska tapa lisätä arvaamattomuutta esimerkiksi peliohjelmiin.

## Näin käytät Fish Shellin satunnaislukujen generointia

```Fish Shell
# Generoidaan yksi kokonaisluku väliltä 1-100
set random_number (random 1 100)
echo "Satunnainen numero 1-100 väliltä: $random_number"

# Generoidaan 10 desimaalilukua väliltä 0-1
for i in (seq 1 10)
  set random_decimal (math random)
  echo "Satunnainen desimaaliluku 0-1 väliltä: $random_decimal"
end
```

**Lähtö:**
```
Satunnainen numero 1-100 väliltä: 47
Satunnainen desimaaliluku 0-1 väliltä: 0.72341991
Satunnainen desimaaliluku 0-1 väliltä: 0.22654887
Satunnainen desimaaliluku 0-1 väliltä: 0.56914746
Satunnainen desimaaliluku 0-1 väliltä: 0.94776117
Satunnainen desimaaliluku 0-1 väliltä: 0.34783025
Satunnainen desimaaliluku 0-1 väliltä: 0.90273647
Satunnainen desimaaliluku 0-1 väliltä: 0.44382513
Satunnainen desimaaliluku 0-1 väliltä: 0.65391913
Satunnainen desimaaliluku 0-1 väliltä: 0.82163593
Satunnainen desimaaliluku 0-1 väliltä: 0.99463275
```

## Syvällistä tietoa satunnaislukujen generoinnista

Fish Shell käyttää satunnaislukujen generoimiseen **rand**-funktiota, joka palauttaa desimaaliluvun väliltä 0-1. Voimme muuttaa tätä lukua kertomalla halutulla tulon välillä, esimerkiksi saatavista lukuja väliltä 100-200 kertomalla rand luvulla 100-200.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Satunnaislukujen generointi Pythonilla](https://realpython.com/python-random/)
- [Lisää vinkkejä ohjelmointiin](https://www.freecodecamp.org/news/)