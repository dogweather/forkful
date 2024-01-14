---
title:                "C: Standardivirhe kirjoittaminen."
simple_title:         "Standardivirhe kirjoittaminen."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelman kirjoittaminen ei ole vain koodien naputtelua, vaan myös virheiden korjaamista ja vianmääritystä. Kirjoittamalla standard erroriin voimme helposti löytää ja tarkistaa virheitä ohjelmassamme.

## Miten

Kuvitellaan, että meillä on yksinkertainen ohjelma, joka laskee kahden luvun summan ja tulostaa sen näytölle. Tässä tapauksessa mahdollisia virheitä voivat olla esimerkiksi yritettäessä jakaa nollalla tai syötettyjen lukujen ollessa merkkijonoja. Mutta kuinka kirjoittaa nämä virheilmoitukset standard erroriin?

Se tapahtuu käyttämällä funktion `fprintf` avulla standard erroriin. Tämä funktio ottaa parametreikseen tiedoston osoittimen ja merkkijonon, jonka haluamme tulostaa. Tiedoston osoitin `stderr` tarkoittaa standard erroria.

```C
fprintf(stderr, "Virhe: Syötetty luku ei ole numero!\n");
```

Yllä olevassa esimerkissä tulostamme virheilmoituksen, jos käyttäjä syöttää merkkijonon sijasta luvun. Tämän ansiosta voimme helposti tunnistaa virheen ja korjata sen ohjelmassa.

## Syvemmälle

Standard erroriin kirjoittaminen antaa meille mahdollisuuden ohjata virheilmoituksia erilliseen paikkaan kuin ohjelman normaali tulostus käyttämällä `stdout`-tiedostoa. Kun ohjelma pyörii esimerkiksi taustalla palvelimena, standard errorista tulostetut virheilmoitukset voidaan ohjata virhelokiin, jotta ne voidaan tarkistaa myöhemmin.

`stderr`-muuttuja on myös hyödyllinen silloin kun meidän tarvitsee kirjoittaa virheilmoituksia funktioissa, joilla ei ole pääsyä `main`-funktioon. Voimme esimerkiksi kutsua `fprintf`-funktiota kirjoittamaan virheilmoituksia tiedostoista, jotka eivät avaudu oikein.

## Katso myös

- [Virheenhallinta C-kielisissä ohjelmissa](https://www.cs.fsu.edu/~myers/c++/notes/errors.html)
- [Standard error C-dokumentaatiossa](https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_73/rtref/stder.htm)