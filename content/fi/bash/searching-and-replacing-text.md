---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Bash: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Hakeminen ja korvaaminen on prosessi, jossa ohjelmoijat korvaavat yhden merkkijonon toisella. Tämä on hyödyllistä silloin, kun halutaan tehdä laajoja muutoksia tekstin sisällössä, tai korvata useita esiintymiä samalla kertaa. Hakeminen ja korvaaminen ovat tärkeitä työkaluja ohjelmoijille, jotka haluavat nopeuttaa työskentelyä ja vähentää inhimillisiä virheitä.

## Kuinka tehdä?
Hakeminen ja korvaaminen voidaan tehdä Bashilla helposti käyttämällä "sed" komentoa. Seuraavassa esimerkissä korvaamme kaikki esiintymät sanalla "kissa" sanalla "koira" tiedostossa nimeltä "teksti.txt":

```Bash
sed -i 's/kissa/koira/g' teksti.txt
```
Tämä komento korvaa kaikki "kissa" sanat "koira" sanoilla teksti.txt -tiedostossa ja tallentaa muutokset tiedostoon.

## Syväsukellus
Hakeminen ja korvaaminen ovat olleet käytössä jo 1970-luvulta lähtien ja ne ovat yksi Bashin tärkeimmistä työkaluista. Nykyään on olemassa myös muita keinoja suorittaa vastaavia operaatioita, kuten tekstieditorit ja tietokoneohjelmistot, mutta Bashin sed-komento pysyy yksinkertaisimpana ja nopeimpana vaihtoehtona.

## Katso myös
Voit lukea lisää Bashista ja sen käyttötavoista Bostwick F. ja Jones J.:n kirjasta "Bash Guide for Beginners". Voit myös tarkistaa Bash-komentorivin Latex Dottorin nettisivuilta, jossa on kattava opas Bashin käytöstä.