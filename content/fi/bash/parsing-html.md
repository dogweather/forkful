---
title:                "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi 

Miksi joku haluaisi käyttää Bash-ohjelmointia HTML-analysoinnissa? Vaikka Bash-maineen kuuluukin olevan tehokas työkalu komentorivillä suoritettaviin tehtäviin, se on myös erinomainen vaihtoehto HTML-sivujen analysointiin. Bashin avulla voit helposti käsitellä HTML-tiedostoja ja erottaa tärkeät tiedot haluamallasi tavalla.

## Miten 

Bashilla on monia hyödyllisiä työkaluja, joilla voit purkaa HTML-tiedostoja ja käsitellä niiden sisältämää tietoa. Alla on esimerkkejä siitä, kuinka Bashin avulla voit selata sivun sisältöä ja tulostaa tietyt elementit.

```
Bash skripti, joka tulostaa kaikki sivun linkit:

```Bash
#!/bin/bash
HEADERS=$(curl -s https://www.example.com/ | grep -o '<a href="[^"]*' | sed 's/<a href="//g')
for PAGE in $HEADERS
do
  echo "$PAGE"
done
```

Esimerkkitulos:

```
/index.html
/about.html
/contact.html
```

```
Bash skripti, joka tulostaa sivun otsikon:

```Bash
#!/bin/bash
TITLE=$(curl -s https://www.example.com/ | grep '<title>' | sed 's/<[^>]*>//g')
echo "$TITLE"
```

Esimerkkitulos:

```
Welcome to Example Website
```

## Syvempi sukellus

Bashia käyttämällä voit myös muokata ja käsitellä HTML-tiedostoja käyttämällä "sed" komentoa. Se on hyödyllinen työkalu, kun haluat vaihtaa tiettyjä tekstiosuuksia HTML-tiedostosta. Voit myös käyttää "grep" komentoa erottamaan haluamasi tiedon, kuten linkit tai otsikot.

Lisäksi voit käyttää Bash-skriptejä, joissa on muita ohjelmointikieliä, kuten Python tai Perl, jotta voit käyttää monimutkaisempia HTML-parsing työkaluja.

## Katso myös 

- [Bash-opas (suomeksi)](https://www.bash.fi/)
- [Sed-opas (englanniksi)](https://www.gnu.org/software/sed/manual/sed.html)
- [Grep-opas (englanniksi)](https://www.gnu.org/software/grep/manual/grep.html)