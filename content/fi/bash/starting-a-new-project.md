---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Uuden projektin aloittaminen tarkoittaa tyhjästä aloittamista, uuden ohjelmiston tai palvelun kehittämistä. Ohjelmoijat tekevät näin haastaakseen itsensä, oppiakseen uutta tai ratkaistakseen jonkin ongelman tai tarpeen.

## Näin teet:
Seuraavassa on esimerkki Bash-skriptistä, joka luo uuden projektikansion ja alustaa tyhjän Git-repositorion.

```Bash
#!/bin/bash
# Uuden projektin luominen
echo "Anna uuden projektin nimi:"
read project_name

mkdir $project_name
cd $project_name
git init
echo "# $project_name" >> README.md
git add README.md
git commit -m "alustava commit"
echo "Uusi projekti luotu!"
```

Kun ajat tämän koodin, se näyttää haun uuden projektin nimen ja luo sitten sen kanssa kansion.

## Syvä sukellus
Projektin aloittaminen bash-skriptin avulla on nykyaikainen tapa, jota ohjelmoijat ovat käyttäneet 2000-luvun alkupuolelta lähtien, kun Bashista tuli yleisimmin käytetty unix-pohjaisten koneiden komentotulkki. Vaihtoehtoisia työkaluja ovat esimerkiksi Make, CMake tai Python-skriptit. Bash-skriptaus tarjoaa kuitenkin helpon ja nopean tavan aloittaa tyhjästä, etenkin kun käytetään versionhallintaa, kuten Git.

## Katso myös
1. Bash Programming -opas: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html
2. Gitin perusteet: https://git-scm.com/book/en/v2/Getting-Started-Git-Basics
3. Projektipohjien luominen Bashissa: https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php