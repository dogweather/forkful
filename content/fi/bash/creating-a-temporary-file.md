---
title:    "Bash: Väliaikaisen tiedoston luominen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

On monia syitä, miksi voit tarvita luoda väliaikaisia tiedostoja Bash-ohjelmoinnissa. Joskus haluat tallentaa väliaikaisesti tietoja, kunnes niitä tarvitaan myöhemmin, tai ehkä tarvitset tilapäisen paikan lukea ja kirjoittaa tiedostoja. Väliaikaiset tiedostot voivat myös olla hyödyllisiä testatessa algoritmeja tai ohjelmakoodia ennen sen integroimista lopulliseen versioon.

## Miten luoda väliaikainen tiedosto Bash-ohjelmoinnissa?

Onneksi Bash-ohjelmointikieli tarjoaa helpon tavan luoda ja käsitellä väliaikaisia tiedostoja. Voit käyttää `mktemp`-komennon avulla luoda väliaikaisen tiedoston, joka on nimetty uniikilla satunnaisella nimellä. Voit myös määrittää tiedoston sijainnin ja halutun tiedoston luontopohjan `mktemp`-komennon avulla.

```Bash
tempfile=$(mktemp) # luo väliaikaisen tiedoston nykyiseen kansioon
echo "Tämä on väliaikainen tiedosto" > $tempfile # kirjoittaa tiedostoon

# tulostaa luodun väliaikaisen tiedoston nimen
echo "Luotu väliaikainen tiedosto: $tempfile"

# poistaa luodun väliaikaisen tiedoston
rm $tempfile
```

Tässä esimerkissä luomme väliaikaisen tiedoston nykyiseen kansioon, kirjoitamme tiedostoon tekstin ja lopuksi poistamme tiedoston. Huomaa, että voit myös määrittää omat tiedostonimi ja tiedostotyyppi `mktemp`-komennolla. 

## Syvemmälle väliaikaisten tiedostojen luomiseen

`mktemp`-komennon lisäksi Bash-ohjelmointikieli tarjoaa muitakin vaihtoehtoja luoda ja käsitellä väliaikaisia tiedostoja. Voit esimerkiksi käyttää `trap`-komennon avulla poistamaan väliaikaisen tiedoston automaattisesti, kun Bash-ohjelman suoritus päättyy. Voit myös määrittää väliaikaisen tiedoston sijainnin ja sudota-ominaisuuden `mktemp`-komennon avulla.

Jos haluat syvempää tietoa väliaikaisista tiedostoista Bash-ohjelmoinnissa, suosittelemme lukemaan Bashin [virallisen dokumentaation](https://www.gnu.org/software/bash/manual/html_node/Temporary-Files.html) aiheesta.

## Katso myös

- [How to Use Temporary Files in a Bash Script](https://linuxize.com/post/bash-temporary-files/) (englanniksi)
- [Bash-historian ja tilapäisten tiedostojen tehokas käyttö](https://opensource.com/article/18/5/bash-tricks-history-processes) (englanniksi)
- [Bash-skriptien perusteet](https://www.shell-tips.com/2006/06/23/bash-traps/) (englanniksi)