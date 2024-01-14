---
title:    "Fish Shell: Komentoriviparametrien lukeminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrit ovat tärkeitä osa Fish Shellin ohjelmoimista. Niiden avulla voit antaa käyttäjille enemmän valinnanvaraa ohjelman suorittamiseen ja muokata sen toimintaa eri tilanteissa. Tässä blogikirjoituksessa käymme läpi, miten lukea komentoriviparametrejä Fish Shellillä ja miten voit hyödyntää niitä omassa ohjelmoinnissasi.

## Kuinka

Käytä ```argparse``` -modulia lukeaksesi komentoriviparametreja Fish Shellissä. Seuraavassa on esimerkki kodista:

```
# Alusta moduli ja määritä parametrit
argparse "Verbose" "V" "Silent" "S"

# Lukee parametrit annettu komentoriviltä
set verbose (argparse -V)
set silent (argparse -S)

# Testi tulostus parametrien mukaan
if test $verbose = 1
    echo "Verbose moodi päällä"
else if test $silent = 1
    echo "Hiljainen moodi päällä"
else
    echo "Ei lisäparametreja annettu"
end
```

Kun suoritamme tämän koodin komentoriviltä esimerkiksi seuraavalla tavalla: ```fish read_arguments.fish -V```, tulostus olisi seuraava:
```
Verbose moodi päällä
```

Voit myös antaa useita parametreja samalla kertaa, esimerkiksi: ```fish read_arguments.fish -V -S```, jolloin tulostus olisi:
```
Ei lisäparametreja annettu
```

Tämä oli yksinkertainen esimerkki komentoriviparametrien lukemisesta Fish Shellillä. Voit myös määrittää muita parametreja, kuten tiedostot tai numerot, ja käyttää niitä omassa ohjelmoinnissasi.

## Syvällinen sukellus

Komentoriviparametrien lukeminen on vain yksi esimerkki siitä, kuinka voit laajentaa Fish Shellin toiminnallisuutta. Voit myös käyttää muita moduuleja, kuten ```getopt``` tai ```docopt```, jotka tarjoavat erilaisia tapoja lukea ja käsitellä parametreja. Voit myös lukea lisää Fish Shellin ohjelmoinnista ja sen ominaisuuksista Fish Shellin verkkosivuilta.

## Katso myös

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Argparse Documentation](https://fishshell.com/docs/current/cmds/argparse.html)
- [Getopt Documentation](https://fishshell.com/docs/current/cmds/getopt.html)
- [Docopt Documentation](https://fishshell.com/docs/current/cmds/docopt.html)