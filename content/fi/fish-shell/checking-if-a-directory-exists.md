---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Fish Shell: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miten tarkistetaan, onko hakemisto olemassa Fish Shell -ohjelmointikielessä?

## Mikä & Miksi?
Tarkistetaan, onko hakemisto olemassa määrittämällä, onko tietty polku pätevä ja käytettävissä ohjelmassa. Ohjelmoijat tekevät tämän välttääkseen virheiden aiheuttaman ohjelman kaatumisen esimerkiksi lukiessaan tai kirjoittaessaan tiedostoja.

## Miten tehdä:
Fish Shell käyttää `test`- ja `-d`-komentoja tämän toiminnallisuuden toteuttamiseksi. Katso alla oleva koodi:

```fish
if test -d /polkusi/hakemiston_nimi
    echo "Hakemisto on olemassa."
else
    echo "Hakemistoa ei ole olemassa."
end
```

Yllä olevan koodin tuloksena:

- Jos hakemisto `/polkusi/hakemiston_nimi` on olemassa, tulostuu teksti `"Hakemisto on olemassa."`
- Jos hakemistoa ei ole olemassa, tulostuu `"Hakemistoa ei ole olemassa."`


## Syväluotaus
Vuonna 2016 lanseeratun Fish Shellin alkuperäinen tavoite oli yksinkertaistaa ja parantaa komentokehotinta. Vaikka yksinkertaisissa skripteissä käytämme edellä mainittua menetelmää tarkistaaksemme, onko hakemisto olemassa, joskus meidän täytyy käsitellä virheitä tai muita monimutkaisia skenaarioita. 

Fish Shell tarjoaa myös `and` ja `or` -operaattoreita, jotka voivat olla hyödyllisiä tällaisissa tilanteissa. Esimerkiksi, jos haluamme tulostaa viestin vain, jos hakemisto on olemassa JA jos voimme kirjoittaa siihen:

```fish
if test -d /polkusi/hakemiston_nimi; and test -w /polkusi/hakemiston_nimi
    echo "Hakemisto on olemassa ja siihen voidaan kirjoittaa."
end
```

Oppimisen ja jatkuvan kehityksen kannalta kannattaa tutustua myös muiden shell-kielten, kuten Bashin tai Zsh:n, samanlaiseen toiminnallisuuteen.

## Katso myös
1. [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
2. [Fish Shellin skriptausopas](https://fishshell.com/docs/3.1/commands.html)
3. [Bash vs Fish -vertailu](https://www.slant.co/versus/2445/2449/~bash_vs_fish)