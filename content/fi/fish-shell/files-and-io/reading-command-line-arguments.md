---
aliases:
- /fi/fish-shell/reading-command-line-arguments/
date: 2024-01-20 17:56:17.063143-07:00
description: "Komennon rivin argumentit ovat ohjelmallesi sy\xF6tettyj\xE4 tietoja.\
  \ Ne mahdollistavat joustavuuden ja mukautettavuuden, jolloin ohjelmasi voi k\xE4\
  ytt\xE4yty\xE4 eri\u2026"
lastmod: 2024-02-18 23:09:08.101147
model: gpt-4-1106-preview
summary: "Komennon rivin argumentit ovat ohjelmallesi sy\xF6tettyj\xE4 tietoja. Ne\
  \ mahdollistavat joustavuuden ja mukautettavuuden, jolloin ohjelmasi voi k\xE4ytt\xE4\
  yty\xE4 eri\u2026"
title: Komennoriviparametrien lukeminen
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)

Komennon rivin argumentit ovat ohjelmallesi syötettyjä tietoja. Ne mahdollistavat joustavuuden ja mukautettavuuden, jolloin ohjelmasi voi käyttäytyä eri tavoin käyttäjän antamien parametrien mukaan.

## How to: (Kuinka tehdä:)

Fish Shellissa argumenttien lukeminen on suoraviivaista. Tässä on esimerkki siitä, miten luet ja käytät argumentteja.

```Fish Shell
function greet
  for arg in $argv
    echo "Hei, $arg!"
  end
end

greet Maailma Kaunis
```

Odota näkeväsi:

```
Hei, Maailma!
Hei, Kaunis!
```

## Deep Dive (Syväsukellus)

Fish Shell tuli alun perin vuonna 2005, tarjoten selkeämmän ja yksinkertaisemman skriptauksen vaihtoehdon. Toisin kuin muut kuoret, kuten Bash tai Zsh, Fishin syntaksi on ytimekäs eikä vaadi $-merkkiä muuttujien edessä. Historiallinen konteksti on tärkeää, sillä se selittää Fishin erilaisia valintoja, joita se käyttää argumenttien käsittelyssä. Verrattuna Bashin `"$@"` tai `"$1"`, Fish käyttää `$argv` listaa, joka toimii kuten Pythonin `sys.argv`.

Vaikka Fish onkin tehty käyttäjäystävällisemmäksi, jotkut skriptit vaativat silti syvempää ymmärrystä siitä, miten `$argv` toimii. Esimerkiksi, voit tarkistaa argumenttien lukumäärän `count $argv` komennolla. Voit myös käyttää `argparse`-toimintoa, kun tarvitaan monimutkaisempia komentosyötteiden käsittelyjä.

## See Also (Katso Myös)

Fish Shell viralliset ohjeet: [docs.fishshell.com](https://fishshell.com/docs/current/index.html)

Argumenttien käsittely Fishissä: [docs.fishshell.com/docs/current/tutorial.html#variables](https://fishshell.com/docs/current/tutorial.html#variables)

Argparse käyttöoikeudet: [docs.fishshell.com/docs/current/cmds/argparse.html](https://fishshell.com/docs/current/cmds/argparse.html)
