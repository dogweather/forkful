---
title:                "Bash: Tekstin etsiminen ja korvaaminen"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi: Miksi etsiminen ja korvaaminen tekstin kanssa on tarpeellista

Etsiminen ja korvaaminen tekstin kanssa on tärkeä osa käyttäjän ja sovelluksen välisen vuorovaikutuksen parantamista. Se auttaa nopeuttamaan työprosesseja ja säästää aikaa manuaaliselta työltä.

## Miten: Esimerkkejä koodista ja tulostaulukoista, jotka on kirjoitettu Bash-kielellä

```
# Etsi ja korvaa teksti sulkumerkkeissä
echo "Tervetuloa (Nimi)!" | sed 's/(.*)/John/' 
```
Tuloste: Tervetuloa John!

```
# Etsi ja korvaa kaikki esiintymät tekstin sisällä
echo "Tämä on testilause." | sed 's/testi/koe/g' 
```
Tuloste: Tämä on koelause.

```
# Tallenna muutokset alkuperäiseen tiedostoon
sed -i 's/hae/haku/g' tiedosto.txt
```
Tämä käsky korvaa kaikki esiintymät "hae" tekstin tiedostossa "haku" tekstillä ja tallentaa muutokset alkuperäiseen tiedostoon.

## Syvä sukellus: Lisätietoa etsimisestä ja korvaamisesta tekstin kanssa

Etsiminen ja korvaaminen tekstin kanssa on tehokas työkalu Bash-ohjelmoinnissa, ja sitä voidaan käyttää erilaisiin tarkoituksiin. Se voidaan yhdistää muihin komentorivin työkaluihin, kuten awk ja grep, jotta voidaan suorittaa monimutkaisempia tekstin käsittelyä. Sed-komento on myös erittäin monipuolinen ja siinä on erilaisia vaihtoehtoja, kuten "s///g" joka korvaa kaikki esiintymät tiedostossa tai "s///1", joka korvaa vain ensimmäisen esiintymän.

## Katso myös
- [Sed-komento: tekstintyöstöura ja sen hyödyt](https://www.tldp.org/LDP/abs/html/textproc.html)
- [RegEx-opas: ilmaisuja ja säännöllisiä lausekkeita Bashissa](https://www.linux.com/training-tutorials/introducing-regular-expression-tutorial-basics/)
- [Grep-komento: tekstinhaku ja sen sovellukset Bashissa](https://www.gnu.org/software/grep/manual/grep.html)