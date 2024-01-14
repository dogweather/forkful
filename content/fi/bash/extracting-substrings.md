---
title:                "Bash: Alimerkkijonojen erottaminen"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien eristäminen on hyödyllinen taito, jota tarvitaan useimmissa Bash-ohjelmoinnin projekteissa. Se mahdollistaa tiettyjen merkkijonojen tai arvojen eristämisen suuremmista tekstimäärästä, mikä on erittäin hyödyllistä tietojen käsittelyssä ja analysoinnissa. 

## Kuinka tehdä

Käytä komentoa `cut` eristämään osia merkkijonoista. Esimerkiksi, jos haluat eristää puhelinnumeron tekstistä "Puhelinnumeroni on +358 123 456 789", voit käyttää seuraavaa koodia:

```Bash 
puhelinnumero="+358 123 456 789"
leikattu_puhelinnumero=$(echo $puhelinnumero | cut -d " " -f 4)
echo $leikattu_puhelinnumero
```

Tämä tulostaisi "123" terminaliin, sillä `cut`-komennolla leikataan puhelinnumerosta ensimmäiset kolme osaa (määritetty `-f` -flagilla) välilyöntien välillä (määritetty `-d` -flagilla).

## Syvemmältä

`cut`-komennon lisäksi on myös muita tapoja eristää substringeja Bash-ohjelmoinnissa. Esimerkiksi, voit käyttää `grep`-komentoa hakemaan tiettyjä merkkijonoja tekstimäärästä ja tallentaa sen muuttujaan:

```Bash 
syntymapaiva="Olen syntynyt 01.01.2000"
eristetty_vuosi=$(echo $syntymapaiva | grep -Eo "[0-9]{4}")
echo $eristetty_vuosi
```

Tämä tulostaisi "2000" terminaliin, sillä `grep` etsii tekstimäärästä numeroyhdistelmiä, jotka koostuvat neljästä numerosta (`[0-9]{4}`). 

## Katso myös

- [Bash-komentorivin opas](https://debianhandbook.info/browse/fi-FI/stable/sect.shell-get.html)
- [Leikkaa komentorivityökalu](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [Grep-komento](https://www.gnu.org/software/grep/manual/grep.html)