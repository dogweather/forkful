---
title:                "Fish Shell: Tulostaminen virheenkorjaustulosteena"
simple_title:         "Tulostaminen virheenkorjaustulosteena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus koodin kehittämisessä voi tulla vastaan tilanne, jossa tarvitsee tarkastella mitä tietoja muuttujissa on juuri tietyllä hetkellä. Tässä tilanteessa debug-tulosteiden tulostaminen voi auttaa hahmottamaan koodin suoritusta ja pääsemään lähemmäksi ratkaisua ongelmaan.

## Miten

Aloita lisäämällä koodiin `set -x`, jotta Fish-shell tulostaa debug-tulosteet.

```
Fish Shell koodi:
set -x
muuttuja=1
toinen_muuttuja="Hei Maailma!"
```

Tässä esimerkissä `set -x` tulostaa muuttujien arvot alapuolella olevaan tulosteeseen:

```
1 muuttuja=1
2 toinen_muuttuja="Hei Maailma!"
```

Voit myös lisätä `echo`-komentoja koodiin niiden osien ja muuttujien tulostamiseksi, joita haluat tarkastella.

```
Fish Shell koodi:
muuttuja=1
echo "Muuttujan arvo on $muuttuja."
toinen_muuttuja="Hei Maailma!"
echo "Toisen muuttujan arvo on $toinen_muuttuja."
```

Tämä tulostaisi seuraavan:

```
Muuttujan arvo on 1.
Toisen muuttujan arvo on Hei Maailma!
```

## Syvempää sukellusta

Fish-shellin `set -x` komento tulostaa vain arvot, mutta voit myös käyttää `set -v` tulostamaan myös muuttujien nimet. Lisäksi voit käyttää `set -l` tulostamaan myös koodirivinumerot, mikä helpottaa debuggaamista suuremmissa koodikokonaisuuksissa.

Voit myös käyttää `set -n` tulostamaan koodin, mutta estämään sen suorittamisen. Tämä on hyödyllistä tarkastellessasi koodin suoritusjärjestystä ja mahdollisia ongelmakohtia.

## Katso myös

- [Fish-shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Debuggaaminen Fish-shellillä](https://fishshell.com/docs/current/tutorial.html#tut_debugging)
- [Fish-shellin keskusteluryhmä](https://github.com/fish-shell/fish-shell/discussions)