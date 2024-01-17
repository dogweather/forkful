---
title:                "Merkkijonon ison kirjaimiksi muuttaminen"
html_title:           "TypeScript: Merkkijonon ison kirjaimiksi muuttaminen"
simple_title:         "Merkkijonon ison kirjaimiksi muuttaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Mikä on isoja kirjaimia ja miksi ohjelmoijat tekevät sitä?
Isoilla kirjaimilla tarkoitetaan merkkijonon muuntamista siten, että kaikki sen kirjaimet ovat isoja. Ohjelmoijat tekevät tätä usein, jotta tiettyjä merkkijonoja voidaan helpommin vertailla ja käsitellä.

## Kuinka tehdä se:
```TypeScript
const string = "Tämä on esimerkkimerkkijono";
const capitalizedString = string.toUpperCase();

console.log(capitalizedString); //TÄMÄ ON ESIMERKKIMERKKIJONO
```

## Syvempi sukellus:
Isojen kirjainten käyttäminen merkkijonoissa juontaa juurensa vanhoista tekstinkäsittelyohjelmista, joissa isoilla kirjaimilla painaminen oli tapa korostaa tai otsikoida tekstiä. Nykyään ohjelmoijat ovat ottaneet tämän käytännön käyttöön esimerkiksi käyttäjän syöttämän tekstin muokkaamisessa. Vaihtoehtoisena tapana muuttaa merkkijonon kaikki kirjaimet isoiksi, on käyttää CSS:ää.

## Lue myös:
Lisätietoja isoista kirjaimista TypeScriptissä löytyy virallisesta käsikirjasta: https://www.typescriptlang.org/docs/handbook/basic-types.html#string-operations