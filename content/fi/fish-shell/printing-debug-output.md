---
title:                "Fish Shell: Virheenjäljitystulosteen tulostaminen"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessamme koodia, yksi tapa löytää ja korjata bugeja on tulostaa debug-viestejä. Tämä auttaa meitä ymmärtämään tarkemmin, mitä koodi tekee ja missä kohtaa on ongelmia. Fish Shellin avulla tämä prosessi on helppo ja tehokas.

## Kuinka tehdä

Fish Shellin avulla voimme helposti tulostaa debug-viestejä käyttämällä sisäänrakennettua `echo` -toimintoa. Samoin kuin muutkin komentotulkkien komennot, se voidaan kirjoittaa käyttämällä putkea, kuten esimerkiksi `echo "Debug-viesti" | less` mikä tulostaa viestin ja samalla avaa sen less-ohjelmassa.

```fish
echo "Tämä on debug-viesti"
```

Jos haluamme lisätä enemmän tietoa viestiin, voimme käyttää erilaisia ​​muuttujia ja sisäänrakennettuja funktionaalisuuksia, kuten `set`, `printf` ja `env`. Nämä antavat meille enemmän hallintaa siitä, mitä haluamme tulostaa.

```fish
set viesti "Tämä on %s-viesti" debug
printf $viesti
env > debug.txt
```

## Syvällinen sukellus

Debug-viestien avulla voimme nähdä vaiheittain, mitä koodimme tekee ja missä kohtaa se saattaa aiheuttaa ongelmia. Voimme myös käyttää ingnostista-tilaa, joka antaa meille tarkempaa tietoa Debug-viestit-järjestelmästä, kuten esimerkiksi missä tiedostossa virhe sijaitsee ja mikä rivi aiheutti sen.

## Katso myös

- [Fish Shellin viralliset dokumentit](https://fishshell.com/docs/current/index.html)
- [Fish Shellin vinkkejä ja temppuja](https://hackernoon.com/fish-shell-is-a-smart-and-user-friendly-command-line-shell-for-linux-mac-and-windows-95d177f5f1f7)
- [Kehittynyt Fish Shell -ohje](https://dev.to/iggystoian/fish-shell-an-in-depth-tutorial-3p0j)