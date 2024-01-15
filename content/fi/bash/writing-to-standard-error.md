---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Bash: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi
Tervetuloa lukemaan Bash-ohjelmoinnin artikkelia, jossa opit miten kirjoittaa standardivirheelle. Tämä taito on hyödyllinen, sillä se auttaa sinua havaitsemaan ja korjaamaan virheitä koodissasi.

## Miten
```Bash
echo "Tämä teksti tulostuu standardivirheelle" 1>&2
```

Kun haluat kirjoittaa jotain standardivirheelle Bash-skriptissäsi, voit käyttää 1>&2 -merkintää. Tämä ohjaa tulostetun tiedon standardivirheen sijaan standarditulostevirtaan. Voit myös käyttää tätä tekniikkaa yhdistettynä muihin komentoihin, kuten grep-välin tai if-lauseiden kanssa.

## Syväsukellus
Standardivirhe on virta, joka kerää kaiken virheellisen tiedon Bash-skriptissäsi. Tämä auttaa sinua havaitsemaan ja korjaamaan virheet nopeasti ennen kuin ne aiheuttavat ongelmia. Voit myös ohjata standardivirheen uudelleen selkeämpään tiedostoon, jolloin voit helpommin tarkistaa virheet ja niiden syyt.

## Katso myös
- [Bash-skriptauksen perusteet](https://linux.die.net/man/1/bash)
- [Standardivirheen hallintatekniikat](https://shapeshed.com/unix-exit-codes/)
- [Bash-skriptien virheenhallintastrategiat](https://linuxcommand.org/lc3_writing_shell_scripts.php#standard_error_and_features_you_want)