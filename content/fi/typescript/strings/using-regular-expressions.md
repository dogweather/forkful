---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:43.594163-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet eli regex ovat tehokas mallin vastaavuuden\
  \ etsimisen ja hakemisen ty\xF6kalu ohjelmoinnissa. Ohjelmoijat k\xE4ytt\xE4v\xE4\
  t regexi\xE4 teht\xE4viin\u2026"
lastmod: '2024-03-13T22:44:56.305663-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet eli regex ovat tehokas mallin vastaavuuden\
  \ etsimisen ja hakemisen ty\xF6kalu ohjelmoinnissa."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Mitä & Miksi?
Säännölliset lausekkeet eli regex ovat tehokas mallin vastaavuuden etsimisen ja hakemisen työkalu ohjelmoinnissa. Ohjelmoijat käyttävät regexiä tehtäviin kuten käyttäjän syötteen validointiin, tekstin etsimiseen tai merkkijonojen manipulointiin, koska se on tehokasta ja monipuolista.

## Kuinka:

Sukelletaan TypeScriptiin ja katsotaan, miten regexiä käytetään yleisiin tehtäviin.

```TypeScript
// Määritellään sähköpostiosoitteen regex-malli
const emailPattern = /\S+@\S+\.\S+/;

// Testataan, vastaako merkkijono sähköpostimallia
const email = "user@example.com";
console.log(emailPattern.test(email)); // Tuloste: true

// Etsitään ja korvataan numerot merkkijonosta
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Tuloste: "Item # costs $#"

// Tiettyjen osien erottaminen merkkijonosta käyttäen kaappausryhmiä
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // Tuloste: "April" "10" "2021"
```

## Syväsukellus

1950-luvulla matemaatikko Stephen Kleene kuvasi säännöllisiä lausekkeita mallina esittämään säännöllisiä kieliä, jotka myöhemmin tulivat oleellisiksi tietojenkäsittelytieteessä. Edetessä regex on kaikkialla ohjelmoinnissa tekstin käsittelyssä.

Vaikka regex on kuin linkkuveitsi merkkijono-operaatioille, ei se ole ilman vaihtoehtoja. Tehtävän monimutkaisuudesta riippuen joskus merkkijonometodit kuten `includes()`, `startsWith()`, `endsWith()` tai jopa jäsentäminen kirjaston avulla voivat olla parempia. Esimerkiksi monimutkaisen JSON-merkkijonon jäsentäminen regexin avulla voi olla painajainen – käytä sen sijaan JSON-jäsentäjää.

Toteutukseen liittyen, regex JavaScriptissä ja TypeScriptissä perustuu ECMAScript-kielispesifikaatioon. Kulissien takana moottorit käyttävät tilakoneita tehokkaaseen mallien vastaavuuteen. On huomionarvoista, että regex-operaatiot voivat olla kalliita suorituskyvyn kannalta, erityisesti huonosti kirjoitettujen mallien kanssa – varo "katastrofaalista takaisinkytkentää".

## Katso Myös

- MDN Web Docs säännöllisistä lausekkeista: [MDN Säännölliset Lausekkeet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Työkalu testata ja debugata regex-malleja [Regex101](https://regex101.com/)
- "Mastering Regular Expressions" -kirja syvälliseen ymmärrykseen: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
