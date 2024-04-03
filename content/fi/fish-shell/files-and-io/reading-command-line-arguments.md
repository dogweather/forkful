---
date: 2024-01-20 17:56:17.063143-07:00
description: "How to: (Kuinka tehd\xE4:) Fish Shellissa argumenttien lukeminen on\
  \ suoraviivaista. T\xE4ss\xE4 on esimerkki siit\xE4, miten luet ja k\xE4yt\xE4t\
  \ argumentteja."
lastmod: '2024-03-13T22:44:57.010509-06:00'
model: gpt-4-1106-preview
summary: Fish Shellissa argumenttien lukeminen on suoraviivaista.
title: Komennoriviparametrien lukeminen
weight: 23
---

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
