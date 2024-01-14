---
title:                "Bash: Lukemassa komentorivin argumentteja"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Bash-ohjelmoinnissa käytetyistä komentoriviparametreista kannattaa lukea nähdäkseen kuinka tehokkaasti voit käsitellä ja suorittaa ohjelmia kanssakäymisessä tietokoneen kanssa.

## Kuinka tehdä

Komentoriviparametrit voidaan lukea merkkijonoilla ` "$" `.

```Bash
# Scripti lukee komentoriviparametreja
nimi="$1" # Ensimmäinen komentoriviparametri tallennetaan muuttujaan nimi
echo "Hei, $nimi!" # Tulostetaan tervehdys käyttäen parametrina annettua nimeä 
```

Suorittaessasi skriptin ja toimitat ensimmäisenä parametrina oman nimesi, tulostetaan viesti "Hei, [nimesi]!" Tämä osoittaa, kuinka helposti voit käyttää komentoriviparametreja osana ohjelmaasi.

## Syvällisempi sukellus

Komentoriviparametreihin liittyy myös muita ominaisuuksia, kuten erilaisten parametrien lukeminen ja niiden arvojen muokkaaminen. Voit myös hyödyntää valmiita komentoja, kuten `getopts` ja `shift`, tehokkaasti komentoriviparametrien käsittelyyn.

## Katso myös

- [Bash-opas (suomeksi)](https://jannekallio.fi/koulutus/bash-perusteet/)
- [Bash-skriptit (englanniksi)](https://devhints.io/bash)
- [Bash-parametrien käyttöohje (englanniksi)](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)