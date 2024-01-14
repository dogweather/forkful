---
title:                "Bash: Sattumanvaraisten numeroiden luominen"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Sattumanvaraisten lukujen luominen voi olla hyödyllistä monissa tilanteissa, kuten koodin testauksessa tai pelien kehittämisessä.

## Miten
Bash-ohjelmointikielen avulla voimme helposti luoda satunnaisia lukuja. Käytämme siihen `shuf` -komentoa, joka ottaa parametrikseen halutun lukujen määrän ja välisen alueen. Alla on esimerkki, jossa luomme satunnaisen numeron väliltä 1-10.

```Bash
numero=$(shuf -i 1-10 -n 1)
echo $numero
```
Tulosteena saamme satunnaisen numeron, kuten esimerkiksi `7`.

Voimme myös luoda satunnaisia merkkijonoja käyttämällä `shuf` -komentoa yhdessä `head` -komenton kanssa. Alla olevassa esimerkissä luomme satunnaisen salasanan, joka koostuu 8 merkistä.

```Bash
salasana=$(shuf -zer -n 8 {A..Z} {a..z} {0..9})
echo $salasana
```

Tulosteena saamme jotain tällaista: `P9aKg8L3`.

## Syväsukellus
Bashilla sattumanvaraisia lukuja luotaessa käytetään yleensä `/dev/urandom` -laitetta, joka tuottaa suuria määriä sattumanvaraisia bittijonoja. Näitä bittejä käytetään lopulta muodostamaan haluttu satunnainen luku.

Lisäksi Bashissa on myös muita komentoja, kuten `rand` ja `jot`, jotka voivat tuottaa satunnaisia lukuja.

## Katso myös
- [DevDocs - Bash `shuf`](https://devdocs.io/bash/shuf)
- [Bash Hackers Wiki - Generating random numbers](https://wiki.bash-hackers.org/commands/builtin/rand)
- [GNU Coreutils `shuf` documentation](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)