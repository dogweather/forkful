---
title:                "Bash: Kirjoittaminen standardivirheeseen"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen (STDERR) on erittäin tärkeä taito Bash ohjelmoijalle. Se antaa mahdollisuuden ohjata virheilmoituksia ja poikkeuksia erilliseen kanavaan, joka ei sekoitu ohjelman normaaliin tulostukseen. Tämä tekee virheenkorjauksesta helpompaa ja auttaa löytämään ja korjaamaan ohjelmien ongelmia nopeammin.

## Miten

Bash-koodissa, voit kirjoittaa standardivirheeseen käyttäen ">&2" operaattoria. Tämä ohjaa stdandardin tulosteen (STDOUT) standardivirheeseen, jolloin virheilmoitukset tulostuvat STDERR-kanavaan. Seuraavassa esimerkissä käytämme komentoa "ls" ja ohjaamme virheilmoitukset standardivirheeseen:

```Bash
ls -d /home/user/non-existing-directory/ >&2
```

Tämä komento tulostaa virheilmoituksen "ls: cannot access '/home/user/non-existing-directory/': No such file or directory" STDERR-kanavaan. Voit myös tulostaa omia virheilmoituksia käyttämällä "echo" komentoa ja ohjaamalla sen STDERR-kanavaan, kuten alla olevassa esimerkissä:

```Bash
echo "Error! File not found." >&2
```

Tämä tulostaa "Error! File not found." virheilmoituksen STDERR-kanavaan. Muista, että oletusarvoisesti kaikki virheilmoitukset ja poikkeukset menevät STDOUT-kanavaan, joten jokaisen komennon jälkeen kannattaa käyttää ">&2" varmistaaksesi, että ne ohjataan oikeaan kanavaan.

## Syvällinen tutustuminen

Kirjoittaminen standardivirheeseen on erittäin hyödyllinen tekniikka, kun halutaan säätää Bash-skriptejä ja ohjelmia. Se erottaa virheilmoitukset ja poikkeukset muusta tulostuksesta, jolloin analysointi ja korjaaminen on helpompaa. Lisäksi, STDOUT ja STDERR voidaan ohjata eri paikkoihin, joten voit tallentaa ne lokitiedostoon ja tutkia niitä myöhemmin.

## Katso myös

- [BASH ohjelmointikieli](https://www.tldp.org/LDP/abs/html/)
- [BASH ohjeet](https://www.gnu.org/software/bash/manual/bash.html)