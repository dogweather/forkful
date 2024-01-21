---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:17.063143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-command-line-arguments.md"
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