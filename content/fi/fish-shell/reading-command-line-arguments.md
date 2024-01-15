---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Fish Shell: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Kun käytämme komentoriviä, usein haluamme suorittaa tietyt toiminnot mahdollisimman nopeasti ja tehokkaasti. Fish Shell mahdollistaa komentorivin yksinkertaisen ja helposti luettavan muodon, joka tekee siitä erinomaisen työkalun koodin suorittamiseen. Tässä artikkelissa näytämme, miten voit lukea komentorivin argumentteja Fish Shell -ohjelmoinnin avulla.

## Kuinka
Fish Shell tarjoaa useita tapoja lukea komentorivin argumentteja. Yksi tapa on käyttää sisäänrakennettuja komentoja kuten `argparse` ja `argv`. Tässä on yksinkertainen esimerkki, joka tulostaa ensimmäisen komentorivin argumentin:

```Fish Shell
argparse -n1
echo $argv[1]
```

Tulostus:

```Fish Shell
Hello
```

Voit myös hakea kaikki komentorivin argumentit kerralla käyttäen `argv`:

```Fish Shell
for arg in $argv
  echo $arg
end
```

Tulostus:

```Fish Shell
Hello
World
```

## Syvälle sukellus
Kuten näemme, Fish Shell tarjoaa käteviä tapoja lukea komentorivin argumentteja. Voit myös käyttää `argparse`-komennon parametreja muuttamaan argumenttien lukemista. Esimerkiksi, jos haluat hakea vain tietyn argumentin indeksin, voit käyttää `-s` parametria:

```Fish Shell
argparse -s2 -n1
echo $argv[1]
```

Tulostus:

```Fish Shell
World
```

Jos haluat käyttää pidempiä argumentteja, voit käyttää `argparse`-komennon `-l` parametria:

```Fish Shell
argparse -n2 -l Hello World
echo $argv
```

Tulostus:

```Fish Shell
Hello World
```

## Katso myös
- [Fish Shellin dokumentaatio] (https://fishshell.com/docs/current/index.html)
- [Fish Shellin GitHub-sivut] (https://github.com/fish-shell/fish-shell)
- [Komentorivin argumentit Pythonissa] (https://realpython.com/python-command-line-arguments/)