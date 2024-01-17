---
title:                "Testien kirjoittaminen"
html_title:           "Bash: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kirjoita testejä ohjelmointikieli Bashilla on varmistusprosessi, jolla tarkistetaan koodin toimivuus ja mahdolliset virheet. Testaaminen auttaa varmistamaan ohjelman laadun ja vähentämään mahdollisia bugeja, jotka voivat aiheuttaa ongelmia käytössä. Se säästää aikaa ja vaivaa korjata ongelmia myöhemmin ja auttaa parantamaan ohjelman laatua yleisesti.

## Miten:

Esimerkkejä testien kirjoittamisesta Bashilla ja näytteen tulosteista  ```Bash ... ``` koodilohkoissa.

```Bash
# Testaa, että tiedosto/luokka on olemassa
if [ -d "$DIRECTORY" ]; then
  echo "$DIRECTORY löytyy."
else
  echo "$DIRECTORY ei löydy."
fi
```

```Bash
# Testaa funktioiden palautusta
sum() {
  echo "$(($1+$2))"
}
if [ $(sum 4 5) -eq 9 ]; then
  echo "Testi läpäisty."
else
  echo "Testi epäonnistui."
fi
```

Tuloste:

"DIREKTORIO löytyy."
"Testi läpäisty."

## Syvemmälle:

Kirjoittaessasi testejä Bashilla, on hyvä olla tietoinen siitä, että aiemmat Bash-versiot eivät tue testirakenteita. Voit kuitenkin käyttää muita testauskirjastoja, kuten `Bats` tai `shunit2`. Voit myös luoda omia testausfunktioita, jotka auttavat varmistamaan haluamasi toiminnallisuudet.

## Katso myös:

- [Bash testikomennot](https://www.tutorialspoint.com/unix_commands/test.htm)
- [Bash-bugit: tunnistaminen ja korjaus](https://ryanstutorials.net/bash-debugging.php)
- [Bats-testauskehys Bashille](https://github.com/sstephenson/bats)
- [shunit2-testauskehys Bashille](https://github.com/kward/shunit2)