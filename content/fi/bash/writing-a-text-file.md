---
title:    "Bash: Tekstitiedoston kirjoittaminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Miksi sitten kirjoittaisit teksti-tiedostoa? Ehkä haluat tallentaa tärkeitä muistiinpanoja tai luoda ohjelman, joka käsittelee tietoja teksti-muodossa. Tekstiedostoja voidaan käyttää moniin eri tarkoituksiin, ja Bash-ohjelmointikielen avulla voit luoda yksinkertaisia, mutta tehokkaita työkaluja käsittelemään näitä tiedostoja.

## Kuinka

Bash on Linux-käyttöjärjestelmän komentokehoteohjelmointikieli, joka mahdollistaa tekstipohjaisen ohjelmoinnin. Alla olevat esimerkit avaavat pienen ikkunan nähdäksesi, kuinka voit luoda ja muokata teksti-tiedostoja Bashin avulla.

```Bash
# Luodaan uusi tiedosto nimeltä "tekstitiedosto.txt"
touch tekstitiedosto.txt

# Kirjoitetaan teksti tiedostoon
echo "Tämä on ensimmäinen rivi" > tekstitiedosto.txt

# Lisätään uusi rivi tiedoston loppuun
echo "Tämä on toinen rivi" >> tekstitiedosto.txt

# Muokataan olemassa olevaa riviä
sed -i 's/toinen kolmas/' tekstitiedosto.txt
```

Tässä esimerkissä luomme uuden tekstitiedoston, kirjoitamme siihen joitain rivejä ja muokkaamme yhden rivin käyttäen `sed`-komennolla. Voit huomata, että käytämme `>` ja `>>` merkkejä kirjoittaaksemme uusia rivejä tiedostoon ja `sed`-komentoa muokataksemme jo olemassa olevaa riviä.

## Syväsukellus

Teksti-tiedostojen kirjoittaminen Bashilla ei rajoitu pelkästään yksinkertaisiin komennoihin, joita näytimme edellä. Bash tarjoaa myös joukon komentoja, jotka helpottavat tiedon käsittelyä, kuten `grep`, `awk` ja `cut`. Voit myös käyttää muuttujia ja ehtolauseita tekstitiedostojen luomisessa ja muokkaamisessa.

## Katso myös

- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Tekstin käsittely Bashilla](https://www.linux.com/tutorials/command-line-text-processing/)