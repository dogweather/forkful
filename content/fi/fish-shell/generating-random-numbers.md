---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Arpova numeroiden luominen on prosessi, jossa tuotetaan satunnaisia numeroita tietokoneilla. Ohjelmoijat käyttävät sitä luomaan ennalta arvaamattomuutta, testaamaan sovelluksia ja simuloimaan tilastotieteellisiä skenaarioita.

## Näin tehdään:

Voit generoida satunnaisluvuja Fish Shellissä käyttämällä komentoa `math`. Tässä on esimerkki:

```fish
set rand (math "random()*100" | math -s 0 "round(\$_)")
echo $rand
```

Tämä koodi arpoo ensin desimaaliluvun 0 ja 100 väliltä, pyöristää sen kokonaisluvuksi ja tulostaa sitten numeron.

## Syvä sukellus

Historiallisesti ottaen numeroita on arvottu käsin, mutta nykyaikaiset ohjelmointikielet tarjoavat sisäänrakennettuja työkaluja tähän tarkoitukseen. Fish Shell käyttää UNIXin [rand()](https://man7.org/linux/man-pages/man3/rand.3.html) -toimintoa randomisoinnissa. Vaihtoehtoisesti voitaisiin käyttää `/dev/random`a, mutta se saattaa olla hitaampi. On syytä muistaa, ettei `math "random()"` generoi salaustason satunnaislukuja.

## Katso myös:

- [Fish Shellin kotisivu](https://fishshell.com/)
- [Man-sivu `math`-komennoille](https://fishshell.com/docs/current/cmds/math.html)
- [Miten generoidaan satunnaislukuja Pythonissa](https://realpython.com/python-random/)