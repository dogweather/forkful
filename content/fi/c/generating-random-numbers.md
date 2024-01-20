---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Satunnaislukujen tuottaminen C-ohjelmoinnissa

## Mikä ja Miksi?

Satunnaislukujen tuottaminen on ohjelmointitehtävä, jossa generoidaan ennustamattomia numeroita määritellyssä joukossa. Ohjelmoijat käyttävät sitä lisätäkseen joustavuutta sekä simuloimaan todennäköisyystapahtumia.

## Näin se tehdään:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(NULL));
    int random_number = rand() % 100;
    printf("%d\n", random_number);
    return 0;
}
```

"Tämä ohjelma tuotetaan satunnaisen numeron välillä 0-99. Käyttäen rand() -funktiota, modulo-operaattoria rajataksesi arvoa ja srand() -funktiota alustamaan pseudosatunnaislukugeneraattorin."

## Syvä sukellus

1. **Historiallinen konteksti:** Ensimmäiset yksinkertaiset satunnaislukugeneraattorit olivat mekaanisia laitteita. Nykyään tietokoneet simuloivat satunnaisuutta käyttäen pseudosatunnaislukugeneraattoreita.

2. **Vaihtoehdot:** C-kielissä on olemassa monia muita menetelmiä satunnaislukujen tuottamiseksi, mukaan lukien uudempi arc4random() macOS:ssä ja BSD.ssä.

3. **Toteutustiedot:** rand() -funktion todellinen toiminta voi vaihdella kääntäjältä toiselle. Useimmissa tapauksissa se käyttää lineaarista yhtälöryhmää.

## Katso myös

1. C Standard Library - [`rand` ja `srand`](http://www.cplusplus.com/reference/cstdlib/rand/)
3. Opengroupin dokumentaatio - [`arc4random`](https://pubs.opengroup.org/onlinepubs/9699919799/functions/arc4random.html)
4. [Lineaaristen yhtälöryhmien](https://en.wikipedia.org/wiki/Linear_congruential_generator) Wikipedia-sivu