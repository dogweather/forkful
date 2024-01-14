---
title:                "Fish Shell: Luen komentoriviparametrit"
simple_title:         "Luen komentoriviparametrit"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi?

Komentoriviparametrit ovat välttämättömiä ohjelmoinnissa ja voivat auttaa sinua suorittamaan monimutkaisia komentosarjoja kätevästi. Tämä blogikirjoitus esittelee, miten voit lukea komentoriviparametrejä käyttäen Fish Shell -ohjelmointikieltä.

## Miten?

Fish Shellin avulla voit lukea komentoriviparametrejä käyttämällä `status` -komennon `argv`-muuttujaa. Tämä muuttuja sisältää kaikki komentoriviparametrit, joten voit käyttää sitä ominaisuuksien ja arvojen hakemiseen. Katso alla olevaa esimerkkiä:

```Fish Shell
# Hakee `status`-komennon `argv`-muuttujan
set komentoriviparametrit $argv

# Tulostaa kaikki komentoriviparametrit yksitellen
for parametri in $komentoriviparametrit
    echo $parametri
end
```

Kun ajat tätä komentoa terminalissa `fishscript.fish -a -b -c`, saat seuraavan tulosteen:

```
-a
-b
-c
```

Voit myös hakea yksittäisen parametrin arvon käyttämällä sen sijaintia `argv`-muuttujassa. Esimerkiksi jos haluat hakea ensimmäisen parametrin, voit käyttää `argv[1]`.

## Syvemmälle

Komentoriviparametrit voivat sisältää myös muita hyödyllisiä tietoja, kuten tiedostonimien ja hakemistojen polkuja. Voit käyttää `status`-komennon muita muuttujia, kuten `argc`, `pid` ja `ppid`, saadaksesi lisätietoja komentoriviparametreistä.

Voit myös luoda omia muuttujia ja sijoittaa niihin komentoriviparametrien arvoja käyttämällä `set` -komentoa. Tämä voi helpottaa tietojen käsittelemistä ohjelmassasi.

## Katso myös

- [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/cmds/set.html)
- [Komentoriviparametriopas](https://www.shell-tips.com/bash/command-line-arguments/) (englanniksi)
- [Fish Shell -tutoriaali](https://fishshell.com/docs/current/tutorial.html)