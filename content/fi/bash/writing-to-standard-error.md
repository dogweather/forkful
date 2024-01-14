---
title:    "Bash: Kirjoittaminen standardivirheeseen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Bash-ohjelmoinnilla on monia etuja, kuten helppokäyttöisyys ja tehokkuus. Suuntaamalla tulosteen standardivirheeseen, voit varmistaa, että virheilmoitukset näkyvät selkeästi ja helposti luettavassa muodossa, mikä helpottaa koodisi vianmääritystä.

## Miten

Tulosteen suuntaaminen standardivirheeseen Bashissa on yksinkertaista käyttämällä ">&2" -merkintää. Tämä osoittaa kaiken tulosteen standardivirheelle sen sijaan, että se menisi standarditulosteeseen. Alla on esimerkki koodi, joka luo virheen ja suuntaa virheilmoituksen standardivirheeseen:

```Bash
#!/bin/bash
echo "Tämä on standardivirhe" >&2
```

Suorittaessa tämän koodin saat seuraavan tulosteen:

`Tämä on standardivirhe`

Kuten näet, virheilmoitus näkyy normaalissa tulostossa sen sijaan, että se piilottuisi muiden tulosteiden joukkoon. Tämä tekee ongelmien havaitsemisesta ja korjaamisesta paljon helpompaa.

## Syvempi sukellus

Bashissa on myös muita tapoja hallita tulostetta, mukaan lukien ohjaaminen tiedostoon tai muuhun prosessiin. Voit myös ohjata tietyn tulosteen standardivirheeseen lisäämällä sen perään "2>&1", mikä ohjaa vain valitun tulosteen standardivirheeseen. Tämä voi olla hyödyllistä esimerkiksi tulosteen suodattamisessa.

Bashissa on myös mahdollista ottaa vastaan standardivirheet ja käsitellä niitä eri tavalla. Tämä auttaa sinua rakentamaan vakaampia ja luotettavampia skriptejä, jotka pystyvät käsittelemään odottamattomia virhetilanteita.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/)
- [Bash-virheet ja virheilmoitukset](https://www.bash-hackers.org/wiki/doku.php/scripting/basherrors)
- [Kuinka ohjata tulostetta Bashissa](https://www.linux.com/tutorials/how-redirect-output-or-errors-bash/)