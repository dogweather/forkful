---
title:    "Bash: Vianetsinnän tulostus"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi printata debuggaus-tulosteita?

Monet kehittäjät käyttävät debuggausta osana ohjelmointiprosessiaan, mutta miksi sitten pitäisi printata debug-tulosteita? Näihin tulosteisiin on helppo lisätä koodin seurantaa ja vianmääritystä, joten ne voivat olla erittäin hyödyllisiä kehitystyössä.

# Näin teet sen:

Useimmissa ohjelmointikielissä on erityisiä komentoja tai funktioita, jotka mahdollistavat debug-tulosteiden printtaamisen. Bashissa voidaan käyttää komentoa "echo" tai "printf" tulostamiseen. Alla on Bash-esimerkki ja sen tuloste:

```Bash
# Määritetään muuttuja
nimi="Matti Meikäläinen"
# Printataan muuttujan sisältö
echo "Tervehdys $nimi!"
```

Tulostus: Tervehdys Matti Meikäläinen!

# Syvempi sukellus:

Debug-tulosteet voi myös formatoida ja lisätä niihin muuta tietoa kuten aikaleimoja ja muuttujien arvoja. Tämä auttaa paremman ymmärryksen ja jäljittämisen kanssa. Alla on esimerkki, jossa käytetään "printf" ja siihen lisätään aikaleima ja muuttujan arvo:

```Bash
# Määritetään muuttuja
numero=42
# Printataan muuttujan sisältö formatoiden
printf "Muuttujan arvo on %s ja aika on %(%H:%M:%S)T" "$numero" `date +%s`
```

Tulostus: Muuttujan arvo on 42 ja aika on 11:34:56

# Katso myös:

- [Bashin echo-komennon dokumentaatio](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Bashin printf-komennon dokumentaatio](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Bash-debuggaus vinkkejä ja temppuja](https://dev.to/thiago641/debugging-utilities-cheatsheet-for-bash-1hbc)