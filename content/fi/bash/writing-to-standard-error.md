---
title:                "Bash: Kirjoittaminen standardi virheisiin."
simple_title:         "Kirjoittaminen standardi virheisiin."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kun ohjelmoit Bash-kielellä, voit käyttää komentoja, joilla voit hallita tiedostojärjestelmää ja suorittaa tehtäviä käyttöjärjestelmässä. Näiden toimenpiteiden tulokset voi usein ohjata standarditähtitteeseen, mutta miksi haluaisit tehdä niin? Standarditähtitteen käyttö voi olla hyödyllistä, kun haluat tallentaa tietoa, jota et halua näyttää tavalliselle käyttäjälle tai tallentaa virheilmoituksia suorituksen aikana.

## Kuinka

Standarditähtitteen käyttö on yksinkertaista. Voit ohjata tulosteen käyttäen "> /dev/stderr" -merkintää komennon lopussa. Esimerkiksi, jos haluat näyttää virheilmoituksen käyttäjälle:

```Bash
ls /tuntematon/hakemisto > /dev/stderr
```

Tämä kääntää hakukuulustelun Bash-kielelle ja ohjaa tuloksen suoraan standarditähtitteeseen. Tämän jälkeen näet virheilmoituksen, joka kertoo, että hakemistoa ei löydy.

## Syventävä sukellus

Standarditähtitteen käyttö voi olla hyödyllistä myös skriptejä kirjoittaessa. Voit lisätä skriptiin tarkoituksella virheen, jotta voit testata, miten se käsittelee virheitä. Voit myös käyttää standarditähtitteen kautta tulostettuja virheilmoituksia hallitaksesi skriptin suorituksen kulkua ja suorittaa haluttuja toimenpiteitä virheiden kohdatessa.

On myös tärkeää muistaa, että standarditähtitteeseen ohjattu tulostus ei näy tavalliselle käyttäjälle. Näin voit suojata tärkeitä tietoja ja estää epätoivottujen tietojen näkyvyyden.

## Katso myös

- [Redirecting Standard Error](https://www.tecmint.com/redirect-stdout-and-stderr-to-file-in-linux/): Ohjeita standarditähtitteen ohjaamiseen tiedostoon Linux-käyttöjärjestelmässä.
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/bash-script.php): Opas Bash-skriptien kirjoittamiseen, jossa käsitellään myös standarditähtitteen käyttöä.
- [Bash Operators](https://www.tldp.org/LDP/abs/html/ops.html): Tietoa erilaisista Bash-kielessä käytetyistä operaattoreista, mukaan lukien standarditähtitee merkintöjen käyttö.