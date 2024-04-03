---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:04.003157-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat malleja, joita k\xE4ytet\xE4\
  \xE4n merkkiyhdistelmien etsimiseen merkkijonoista. Ohjelmoijat hy\xF6dynt\xE4v\xE4\
  t niit\xE4 tekstin ja datan\u2026"
lastmod: '2024-03-13T22:44:56.085691-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat malleja, joita k\xE4ytet\xE4\
  \xE4n merkkiyhdistelmien etsimiseen merkkijonoista."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Miten:
Säännöllisten lausekkeiden käyttö Google Apps Scriptissä on suoraviivaista JavaScript-pohjaisen syntaksin ansiosta. Näin voit sisällyttää regexiä skripteihisi yleisiin tehtäviin kuten etsintään ja datan validointiin.

### Merkkijonojen etsintä
Oletetaan, että haluat löytää, sisältääkö merkkijono tietyn mallin, kuten sähköpostiosoitteen. Tässä on yksinkertainen esimerkki:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Löytyi: " + found[0]);
  } else {
    Logger.log("Sähköpostia ei löydetty.");
  }
}

// Esimerkin käyttö
findEmailInText("Ota yhteyttä osoitteeseen info@example.com.");
```

### Datan validointi
Säännölliset lausekkeet loistavat datan validoinnissa. Alla on funktio, joka validoi syötemerkkijonon tarkistaen, noudattaako se yksinkertaista salasanapolitiikkaa (vähintään yksi iso kirjain, yksi pieni kirjain ja vähintään 8 merkkiä).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Esimerkkituloste
Logger.log(validatePassword("Str0ngPass")); // Tulostaa: true
Logger.log(validatePassword("heikko"));      // Tulostaa: false
```

## Syväsukellus
Google Apps Scriptissä käytettävät säännölliset lausekkeet periytyvät JavaScriptistä, joka standardoitiin ensimmäisen kerran ECMAScript-kielispesifikaatiossa kesäkuussa 1997. Vaikka ne ovat voimakkaita, ne voivat joskus johtaa hämmentävään ja vaikeasti ylläpidettävään koodiin, erityisesti kun niitä käytetään liikaa tai monimutkaisiin mallien tunnistamistehtäviin, jotka voidaan ratkaista tehokkaammin käyttämällä muita jäsentämismenetelmiä.

Esimerkiksi, vaikka regexiä voidaan käyttää HTML- tai XML-jäsentämiseen tiukassa paikassa, sen käyttöä yleensä vältetään näiden dokumenttien sisäkkäisten ja monimutkaisten rakenteiden takia. Sen sijaan erityisesti tällaisten rakenteiden jäsentämiseen suunnitellut työkalut, kuten HTML:lle suunnitellut DOM-jäsentimet, ovat luotettavampia ja luettavampia.

Lisäksi Google Apps Script -kehittäjien tulisi olla tietoisia mahdollisista suorituskykyongelmista käyttäessään monimutkaisia regex-malleja suurimittaisten tekstimanipulointitehtävien yhteydessä, koska regex-käsittely voi olla CPU-intensiivistä. Tällaisissa tapauksissa tehtävän jakaminen yksinkertaisempiin alitehtäviin tai sisäänrakennettujen merkkijonomanipulaatiofunktioiden käyttö voisi tarjota paremman tasapainon suorituskyvyn ja ylläpidettävyyden välillä.
