---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:02:49.074949-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Uuden projektin aloittaminen on puhtaan koodisivun luomista ja projektikansion rakentamista. Koodarit aloittavat uusia projekteja organisoidakseen ideansa ja aloittaakseen puhtaalta pöydältä.

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
