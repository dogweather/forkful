---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Komentorivin argumenttien lukemisella tarkoitetaan ohjelman syötteiden vastaanottamista komentoriviparametrien, kuten tiedostojen nimien tai muiden arvojen kautta. Ohjelmoijat käyttävät tätä antaakseen komentoriviltä ohjelmalle tiedot, jotka ohjaavat sen toimintaa.

## Näin se toimii:
Bash-skriptissä komentorivin argumentit on tallennettu erityisiin sijainneihin, jotka on merkitty dollarimerkillä ja numerolla (`$0`, `$1`, `$2`, jne.). `$0` sisältää skriptin nimen ja muut (`$1`, `$2`, jne.) sisältävät komentorivin argumentit.

```Bash
#!/bin/bash
# Tämä on esimerkkiskripti.
echo "Skriptin nimi: $0"
echo "Ensimmäinen argumentti: $1"
echo "Toinen argumentti: $2"
```

Käyttäjä voi suorittaa skriptin komentoriviltä syöttämällä sen nimen ja argumentit. Esimerkki:

```Bash
./esimerkkiskripti.sh parametri1 parametri2
```

Tulostus olisi seuraavanlainen:

```Bash
Skriptin nimi: ./esimerkkiskripti.sh
Ensimmäinen argumentti: parametri1
Toinen argumentti: parametri2
```

## Syvempi sukellus
Bash-skriptit saivat alkunsa Unix-käyttöjärjestelmässä 1970-luvulla. Niiden tarkoituksena oli helpottaa järjestelmänhallintaa automatisoimalla toistuvia tehtäviä.
Vaihtoehtoisina menetelminä voit käyttää getopts-funktiota monimutkaisten komennojen käsittelyyn tai voit käydä läpi argumentit silmukassa.

Unix-lähtöisissä järjestelmissä luettaessa komentorivin argumentteja ne kulkevat ensin käyttöjärjestelmän kutsupinon läpi ennen skriptiin saapumista. Tämä mahdollistaa viime hetken muutokset, kuten paikan korvaamisen dynaamisilla arvoilla.

## Katso myös
Bash-skriptauksen aloittamiseen voi tutustua seuraavien linkkien kautta:
- GNU:n virallinen Bash-referenssi: https://www.gnu.org/software/bash/manual/bash.html
- Komentorivin argumenttien käsittely Bashissa: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_01.html
- Getopts-komennon käyttö: https://www.tldp.org/LDP/abs/html/internalgetopts.html