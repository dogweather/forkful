---
date: 2024-01-20 18:02:49.074949-07:00
description: "How to: (Kuinka tehd\xE4:) Historiallisesti koodarit k\xE4yttiv\xE4\
  t paperia piirt\xE4\xE4kseen koodin rungon ennen tietokoneille siirtymist\xE4. Nyt\
  \ k\xE4yt\xE4mme\u2026"
lastmod: '2024-04-05T22:51:10.893096-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Historiallisesti koodarit k\xE4yttiv\xE4t paperia piirt\xE4\
  \xE4kseen koodin rungon ennen tietokoneille siirtymist\xE4."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: (Kuinka tehdä:)
```Bash
# Luo uusi kansio projektille
mkdir miinuusiiprojekti

# Siirry uuteen kansioon
cd miinuusiiprojekti

# Alusta Git-repositorio (valinnainen)
git init

# Luo perustiedostot
touch README.md main.sh

# Tarkista kansio
tree
```
Sample output:
```Bash
.
├── README.md
└── main.sh

0 directories, 2 files
```

## Deep Dive (Syväsukellus)
Historiallisesti koodarit käyttivät paperia piirtääkseen koodin rungon ennen tietokoneille siirtymistä. Nyt käytämme kansiorakenteita ja versionhallintaa projektien hallintaan. Vaihtoehtoja ovat erilaiset projektinhallintatyökalut, kuten Yeoman tai Cookiecutter, jotka automatisoivat rakenteen. Implementaation yksityiskohdat vaihtelevat työkalujen ja ympäristön mukaan, mutta perusajatus on luoda looginen ja toimiva runko projektin eri osille alusta lähtien.

## See Also (Katso Myös)
- [Git - perusteet](https://git-scm.com/book/fi/v2/Aloittaminen-Gitin-perusteet)
- [Bash-skriptauksen ohje](https://www.gnu.org/software/bash/manual/)
