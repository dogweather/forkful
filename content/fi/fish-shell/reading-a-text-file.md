---
title:                "Tiedoston lukeminen"
html_title:           "Fish Shell: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä varten

Oletko ikinä törmännyt tekstiin, josta haluat etsiä tai muokata jotain tiettyä sanaa tai lausetta? Fish Shellin avulla voit lukea tekstitiedostoja suoraan komentokehotteesta, joten voit helposti etsiä ja muokata tiedostoa ilman, että sinun tarvitsee avata erillistä tekstieditoria.

## Miten

Fish Shellin käyttäminen tekstitiedostojen lukemiseen ja muokkaamiseen on helppoa! Sinun tarvitsee vain kirjoittaa komentoriville `cat` ja sen jälkeen tiedoston nimi, jonka haluat lukea. Esimerkiksi, jos haluat lukea tiedoston nimeltä "testi.txt", kirjoita seuraava komento:

```Fish Shell
cat testi.txt
```

Kun olet kirjoittanut komennon, paina "Enter" ja voit nähdä tiedoston sisällön komentokehotteessa.

Mutta mitä jos haluat etsiä tiettyä sanaa tai lausetta tiedostosta? Voit käyttää komentoa `grep`, joka etsii annetun sanan tai lauseen tiedostosta ja näyttää rivit, joilla se esiintyy. Esimerkiksi, jos haluat etsiä sanaa "fish" tiedostosta "testi.txt", kirjoita seuraava komento:

```Fish Shell
grep fish testi.txt
```

Tämä näyttää kaikki rivit, joilla sana "fish" esiintyy tiedostossa.

## Syvällä sukellus

Fish Shell tarjoaa monia erilaisia komentoja tiedostojen lukemiseen ja muokkaamiseen, mukaan lukien `head` ja `tail`, jotka näyttävät tiedoston ensimmäiset tai viimeiset rivit, `wc`, joka laskee tiedoston rivit, sanat ja merkit, ja `sed`, joka muokkaa tiedoston sisältöä.

Voit myös yhdistää erilaisia komentoja ja käyttää putkimerkkiä (`|`) siirtääksesi tiedoston läpi useamman komennon. Esimerkiksi, jos haluat nähdä tiedoston ensimmäiset 10 riviä ja sitten laskea, kuinka monta sanaa tiedostossa on, voit yhdistää `head`- ja `wc`-komentoja seuraavasti:

```Fish Shell
head -n 10 testi.txt | wc -w
```

Tämä näyttää 10 ensimmäistä riviä tiedostosta ja laskee sitten niiden sanojen lukumäärän. 

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Kattava opas Fish Shellin käyttöön](https://medium.com/@arpitbhayani/the-ultimate-guide-to-fish-shell-3791c6bcc57a)
- [Fish Shellin opetusohjelma Aloittelijoille](https://dev.to/angadsharma1016/fish-shell-tutorial-for-beginners-1bpp)