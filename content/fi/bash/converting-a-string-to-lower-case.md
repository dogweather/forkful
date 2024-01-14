---
title:    "Bash: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaisi muuttaa merkkijonon pienaakkosiksi? Yksinkertaisesti sanottuna, se voi auttaa tekstin vertailussa tai kauniiden tulosteiden luomisessa.

## Miten
Tämä artikkeli käsittelee kuinka muuttaa Bash-skriptissä olevan merkkijonon pienaakkosiksi.
 
```Bash 
str="Tämä On Merkkijono"
echo "Pienaakset ovat: ${str,,}"
```
Tämä tulostaa:
`Pienaakset ovat: tämä on merkkijono`


Merkkijonon muuttaminen pienaakkosiksi on helppoa Bashissa käyttämällä syntaksia `${variable,,}`. Tämä tapahtuu asettamalla valittu muuttuja ensimmäisellä kirjaimella "$" -merkillä ja lisäämällä kaksi pilkkua jäljessä. Lauseketta voidaan käyttää myös leikkaamaan merkkijonoja numeroiden, erikoismerkkien tai sanojen perusteella.

## Syväsukellus
Bashissa tulee useita sisäänrakennettuja merkkijonotoimintoja, jotka helpottavat kaikkien kirjainten muuttamista pienaakkosiksi. Käyttämällä `tr` -komentoa voidaan muuttaa merkkijonon kaikki kirjaimet pienaakkosiksi ja `sed`-komennolla voidaan muuttaa vain ensimmäinen kirjain. Kaikkien kirjainten muuttamiseksi pienaakkosiksi voidaan myös käyttää komentoa `awk` ja `tr '[A-Z]' '[a-z]'` joka tulostaa merkkijonon pienaakkosina. Bashilla on myös komento `fold` joka muuttaa merkkijonon suurin osa pienaakkosiksi.

## Katso myös
- Bashin virallinen dokumentaatio merkkijonojen manipulointiin: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion
- Bash-skriptien opetusohjelma, joka käsittelee merkkijonojen käsittelyä: https://debian-administration.org/article/316/Manipulating_strings_in_Bash
- Pienaksi muuttamisen syvempää tarkastelua: http://tldp.org/LDP/abs/html/string-manipulation.html