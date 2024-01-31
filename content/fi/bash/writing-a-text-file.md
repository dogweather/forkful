---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedostojen kirjoittaminen tarkoittaa tietojen tallentamista helposti luettavassa muodossa. Ohjelmoijat tekevät tämän datan säilömiseksi, lokien kirjaamiseksi ja konfiguraatioiden hallitsemiseksi.

## How to:
Teksitiedoston kirjoittaminen ja lisäys peruskomennolla:
```Bash
echo "Tämä on tekstirivi" > tiedosto.txt # Luo uuden tiedoston tai korvaa olemassaolevan sisällön
echo "Lisää tekstiä" >> tiedosto.txt    # Lisää tekstiä tiedostoon

cat tiedosto.txt # Tulostaa tiedoston sisällön näytölle
```
Output:
```
Tämä on tekstirivi
Lisää tekstiä
```

Luodaan skripti tiedoston kirjoittamiseen:
```Bash
#!/bin/bash
tiedoston_nimi="raportti.txt"
teksti="Tarkistus suoritettu: $(date)"
echo $teksti > $tiedoston_nimi
```

## Deep Dive
Ennen graafisten käyttöliittymien aikaa kaikki tiedostojen käsittely tapahtui komentoriviltä. `echo` ja `cat` ovat yksinkertaisia ja nopeita työkaluja tiedoston käsittelyyn; `echo` lähettää annetun tekstin standardiulostuloon tai tiedostoon, ja `cat` (concatenate) yhdistää ja tulostaa tiedostoja. `>` on ylikirjoitusmodaattori ja `>>` lisäysmodaattori. Modernit skriptauskielet kuten Python ja Ruby tarjoavat monipuolisempia tiedostokäsittelytoimintoja, mutta Bash-skriptaus on edelleen yleistä nopeiden ja yksinkertaisten toimintojen automatisointiin Unix-pohjaisissa järjestelmissä.

## See Also
- GNU Core Utilities: https://www.gnu.org/software/coreutils/
- Bash-skriptausopas: https://www.gnu.org/software/bash/manual/bash.html
- Linux-komentorivin perusteet (kirja): https://www.linuxcommand.org/tlcl.php
