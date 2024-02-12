---
title:                "Koodin refaktorointi"
aliases:
- /fi/typescript/refactoring/
date:                  2024-01-26T03:37:02.980764-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/refactoring.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista toimintaa. Ohjelmoijat tekevät sitä tehdäkseen koodista selkeämpää, ylläpidettävämpää ja vähentääkseen monimutkaisuutta, mikä tekee siitä helpommin ymmärrettävän henkilölle, joka sukeltaa siihen uutena.

## Miten:
Oletetaan, että sinulla on TypeScript-funktio, joka on nähnyt parempia päiviä - se on hieman sekasotku ja kaipaisi hieman hellää huolenpitoa:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Refaktoroidussa muodossa tämä saattaisi näyttää tältä:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

Toinen esimerkki on robustimpi, hyödyntäen TypeScriptin tyypitysjärjestelmää `interface`:n avulla välttääkseen potentiaaliset suoritusaikaiset virheet ja parantaakseen luettavuutta.

## Syväsukellus
Refaktorointi ei ole moderni käsite; se on kehittynyt ohjelmoinnin mukana, muodollistuen Martin Fowlerin kirjan "Refactoring: Improving the Design of Existing Code" julkaisun myötä vuonna 1999. Se on elintärkeää ketterässä kehitysympäristössä, mahdollistaen joustavat koodimuutokset. Manuaalisen refaktoroinnin vaihtoehtoja ovat automatisoidut työkalut kuten TSLint tai TypeScriptin oma kielipalvelin, jotka voivat ehdottaa tai jopa suorittaa tietyt refaktorointitehtävät puolestasi. Toteutuksen yksityiskohdat yleensä sisältävät "koodinhajujen", kuten duplikaattikoodin, pitkien metodien tai suurten luokkien, tunnistamisen ja kuvioiden soveltamisen korjaukseen—kuten metodien eriyttäminen, siirtäminen sopivampiin luokkiin tai yksinkertaisempien rakenteiden käyttö. Nämä kuviot ovat keskeisiä ymmärtäessä refaktoroinnin mitä ja miksi.

## Katso Myös
- [Kirja "Refactoring: Improving the Design of Existing Code" kirjoittanut Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint staattiselle koodianalyysille](https://palantir.github.io/tslint/)
- [Koodinhajujen ymmärtäminen](https://refactoring.guru/refactoring/smells)
