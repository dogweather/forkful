---
title:                "Bash: Tarkistetaan, jos hakemisto on olemassa"
simple_title:         "Tarkistetaan, jos hakemisto on olemassa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Tarkista, onko hakemistoa olemassa

Bash on yksi suosituimmista komentokehotteiden kielistä Linux- ja Unix-järjestelmissä. Se tarjoaa monia hyödyllisiä työkaluja, mukaan lukien mahdollisuuden tarkistaa, onko tietty hakemisto olemassa. Usein tämä voi olla tarpeellista, kun suoritetaan skriptejä tai ohjelmia, jotka vaativat tietyn hakemiston olemassaolon varmistamista ennen toiminnon suorittamista.

## Miten tehdä se

Tarkistaaksesi, onko hakemisto olemassa Bashissa, voit käyttää komentoa "test -d", jota seuraa hakemiston nimi, jonka haluat tarkistaa. Esimerkiksi, jos haluat tarkistaa, onko hakemisto nimeltä "asiakirjat" olemassa, käyttäisit seuraavaa komentoa:

```Bash
test -d asiakirjat
```

Tämä komento palauttaa arvon "true", jos hakemisto on olemassa, ja "false", jos hakemistoa ei löydy. Voit myös tallentaa tämän palautetun arvon muuttujaan ja käyttää sitä koodissasi jatkotoimia varten.

Voit myös käyttää "if" -lauseketta:

```Bash
if test -d asiakirjat
then
	echo "Hakemisto on olemassa."
else
	echo "Hakemistoa ei löydy."
fi
```

Tämä koodi tulostaa joko "Hakemisto on olemassa." tai "Hakemistoa ei löydy.", riippuen siitä, onko hakemisto olemassa vai ei.

## Syvempi sukellus

Kun käytät komentoa "test -d" tarkistaaksesi hakemiston olemassaolon, se tarkistaa vain, onko hakemisto todella olemassa. Tämä ei kuitenkaan välttämättä tarkoita, että hakemisto on käytettävissä tai että siihen pääsee. Jos esimerkiksi käytät komentoa "test -d" ulkoisen tai irrotettavan levyn hakemistoon, se voi antaa arvon "true", vaikka levyä ei olisikaan kytketty tietokoneeseen.

Tässä tapauksessa voit lisätä komennon "test -w" tarkistaaksesi, onko hakemistoon kirjoitusoikeudet. Käyttämällä näiden kahden komennon yhdistelmää voit varmistaa, että hakemisto on sekä olemassa että käytettävissä.

```Bash
if test -d asiakirjat && test -w asiakirjat
then
	echo "Voit käyttää hakemistoa."
else
	echo "Et voi käyttää hakemistoa."
fi
```

## Katso myös

- [Bash-komentoriviopas](https://www.linux.com/learn/learn-bash-command-line)
- [Linuxin hakemistonhallinta](https://www.tecmint.com/linux-directory-structure-and-important-files-paths-explained/)
- [Bash-skriptikirjasto](https://www.shellscript.sh/)