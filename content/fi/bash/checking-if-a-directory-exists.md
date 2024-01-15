---
title:                "Tarkistaako hakemisto on olemassa"
html_title:           "Bash: Tarkistaako hakemisto on olemassa"
simple_title:         "Tarkistaako hakemisto on olemassa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi tarkistaa onko kansio olemassa?

Tarkistamalla ensin, voit välttää törmäämästä virheisiin, jos ohjelma yrittää käyttää kansiota, jota ei ole olemassa. Tämä auttaa myös varmistamaan, että koodisi toimii halutulla tavalla ja voi helpottaa korjausten tekemistä, jos tarpeen.

## Miten

Bashissa on useita tapoja tarkistaa, onko kansio olemassa. Yksi tapa on käyttää "test" -komentoa, jossa annetaan parametri "-d" (directory). Jos palautettu arvo on "true" (1), kansio on olemassa.

```Bash
if [ -d "kansio" ]; then
  echo "Kansio on olemassa"
fi
```

Toinen tapa on käyttää "ls" -komentoa ja ohjata sen tulos loppuunohjauksen avulla:

```Bash
if ls "kansio" > /dev/null 2>&1; then
  echo "Kansio on olemassa"
fi
```

Lopuksi, voit myös käyttää "stat" -komentoa, joka palauttaa tiedot halutusta kansioista ja tarkistaa, onko se olemassa:

```Bash
if [ -e "kansio" ]; then
  echo "Kansio on olemassa"
fi
```

## Syväsukellus

Tarkemmin sanottuna, "test" -komento tarkistaa, onko tiedostosi olemassa ja palauttaa "true" tai "false" (0 tai 1). "-d" -parametrilla annetaan tiedolle lisäpätevyys, joka tarkistaa, onko se kansio.

"ls" -komento listaa kaikki tiedostot ja kansiot määritetyssä polussa ja ohjaa tuloksen loppuunohjauksen kautta "/dev/null" -tiedostoon, estäen näkyvyyden tulosteen.

"stat" -komento palauttaa tiedoston tiedot ja jos se palauttaa tiedoston tiedot, se tarkoittaa, että tiedosto on olemassa.

See Also
- [Bash Manual - Test Command](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)
- [Stack Overflow - How to check if a directory exists in a Bash Shell Script](https://stackoverflow.com/questions/59838/how-to-check-if-a-directory-exists-in-a-shell-script)