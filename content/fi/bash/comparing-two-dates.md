---
title:    "Bash: Kahden päivämäärän vertailu"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Aika-vertausten tekeminen on tärkeää ohjelmoinnissa, sillä se mahdollistaa tietyn ajankohdan suhteellisen vertailun. Tämä voi olla tarpeellista esimerkiksi tietueiden järjestämisessä tai tiettyjen tapahtumien käsittelemisessä. Bash-ohjelmoinnissa on helppo vertailla kahta päivämäärää, ja tässä artikkelissa näytämme miten se tehdään.

## Miten tehdä

Vertailemiseksi tarvitaan komento `test`, jonka avulla voi tarkistaa ehtoja.

```
Bash
if test $date1 -gt $date2
then
     echo "$date1 on myöhempi kuin $date2"
else
     echo "$date2 on myöhempi kuin $date1"
fi
```

Tässä esimerkissä `test`-komennon avulla vertaillaan kahta muuttujaa, `date1` ja `date2`. Jos `date1` on suurempi kuin `date2`, tulostetaan ensimmäinen viesti, muuten tulostetaan toinen viesti.

## Syvemmälle

Bash-ohjelmointikielen `test`-komennon avulla voidaan vertailla myös muita aikajärjestelmän arvoja, kuten tiedostojen muokkauspäiviä tai kelloaikoja. Vertailussa käytetään usein seuraavia argumentteja:

- `-eq` tarkistaa, ovatko kaksi arvoa yhtä suuret
- `-ne` tarkistaa, ovatko kaksi arvoa erisuuret
- `-lt` tarkistaa, onko ensimmäinen arvo pienempi kuin toinen
- `-le` tarkistaa, onko ensimmäinen arvo pienempi tai yhtä suuri kuin toinen
- `-gt` tarkistaa, onko ensimmäinen arvo suurempi kuin toinen
- `-ge` tarkistaa, onko ensimmäinen arvo suurempi tai yhtä suuri kuin toinen

Tässä esimerkissä käytetään `-nt` argumenttia, joka tarkistaa, onko ensimmäinen tiedosto uudempi kuin toinen:

```
Bash
if test $file1 -nt $file2
then
     echo "$file1 on uudempi kuin $file2"
else
     echo "$file2 on uudempi kuin $file1"
fi
```

Voit myös vertailla kokonaisia päivämääriä ja kellonaikoja käyttämällä `date`-komentoa:

```
Bash
date1=$(date -d "2020-06-01 13:00" +%s)
date2=$(date -d "2020-06-05 10:30" +%s)

if test $date1 -gt $date2
then
     echo "Ensimmäinen päivämäärä on myöhempi kuin toinen"
else
     echo "Ensimmäinen päivämäärä on aikaisempi kuin toinen"
fi
```

Tässä esimerkissä `date`-komento muuntaa päivämäärät sekunneiksi `%s`-muotoon, jotta niitä voidaan vertailla.

## Katso myös

- [GNU Bash manuaali](https://www.gnu.org/software/bash/manual/bash.html)
- [Linux-komentorivin ohjeet](https://help.ubuntu.com/community/UsingTheTerminal)
- [Bash-ohjelmointikielen perusteet](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html)