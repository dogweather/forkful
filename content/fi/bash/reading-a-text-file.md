---
title:                "Tekstitiedoston lukeminen"
html_title:           "Bash: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Jotenkin on aika tuttu tilanne: sinulla on valtava teksti-tiedosto, joka sisältää kaikki tärkeät tiedot, mutta sinun täytyy löytää tietty tieto sieltä. Näin saat tietää, miten voit lukea teksti-tiedostoja Bashilla ja helpottaa elämääsi.

## Kuinka
Bash on erittäin hyödyllinen ohjelmointikieli ja käy kätevästi kaikkeen komentoon-perustuvaan tiedon käsittelyyn, kuten teksti-tiedostojen lukemiseen. Tässä on yksinkertainen esimerkki:

```Bash
while read line; do        # silmukka lukee rivin kerrallaan
  echo $line              # tulostaa luettavan rivin
done < tiedosto.txt       # ohjaa tiedoston sisältö silmukkaan
```

Tämä koodi lukee tiedoston rivi kerrallaan ja tulostaa sen sisällön. Voit myös tallentaa luettavat tiedot muuttujaksi ja käyttää niitä myöhemmin:

```Bash
while read line; do        # silmukka lukee rivin kerrallaan
  data=$line              # tallentaa luettavan rivin muuttujaan
  # tee jotakin data-muuttujalla
done < tiedosto.txt       # ohjaa tiedoston sisältö silmukkaan
```

## Syvä Sukellus
Bashilla on monia tapoja lukea teksti-tiedostoja, kuten `cat`, `grep` ja `awk` komennot. Voit käyttää näitä komentoja yhdessä erilaisilla parametreilla saadaksesi haluamasi tuloksen. Esimerkiksi, voit käyttää `grep` komentoa löytääksesi tietyn sanan tai ilmauksen tiedostosta:

```Bash
grep "etsittävä_sana" tiedosto.txt       # tulostaa rivit, joissa esiintyy sana
```

Voit myös ohjata tulosteen uuteen tiedostoon `>`-merkin avulla:

```Bash
grep "etsittävä_sana" tiedosto.txt > uusi_tiedosto.txt    # tallentaa tuloksen uuteen tiedostoon
```

## Katso myös:
- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/)
- [Vinkkejä Bash-ohjelmointiin](https://linuxhint.com/bash_programming_tutorial/)
- [Oppaita tekstimuotoisten tiedostojen käsittelyyn Bashilla](https://www.digitalocean.com/community/tutorials/how-to-use-bash-to-manipulate-text-in-files)