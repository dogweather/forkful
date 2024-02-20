---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:03.148817-07:00
description: "Associatieve arrays, bekend als objecten in Google Apps Script (een\
  \ variant van JavaScript), stellen programmeurs in staat om verzamelingen van sleutel-\u2026"
lastmod: 2024-02-19 22:05:09.410437
model: gpt-4-0125-preview
summary: "Associatieve arrays, bekend als objecten in Google Apps Script (een variant\
  \ van JavaScript), stellen programmeurs in staat om verzamelingen van sleutel-\u2026"
title: Gebruik van associatieve arrays
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, bekend als objecten in Google Apps Script (een variant van JavaScript), stellen programmeurs in staat om verzamelingen van sleutel-waardeparen te creëren. Deze functionaliteit is cruciaal voor het efficiënt opslaan en manipuleren van gegevens, vooral bij het werken met dynamisch benoemde eigenschappen of wanneer het lineaire opslag- en toegangsmodel van een traditionele array niet volstaat.

## Hoe:

In Google Apps Script creëer en manipuleer je associatieve arrays (objecten) met behulp van accolades `{}`, waarbinnen je sleutel-waardeparen definieert. Sleutels zijn unieke identificatoren en waarden kunnen variëren van strings en getallen tot meer complexe objecten of functies. Hier is een basisvoorbeeld:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Waarden benaderen
  Logger.log(user.name); // Geeft uit: John Doe
  Logger.log(user["email"]); // Geeft uit: johndoe@example.com

  // Nieuwe sleutel-waardeparen toevoegen
  user.title = "Softwareontwikkelaar";
  user["country"] = "VS";

  Logger.log(user.title); // Geeft uit: Softwareontwikkelaar

  // Itereren over sleutel-waardeparen
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Een voorbeelduitvoer voor het iteratiedeel zou er zo uit kunnen zien:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Softwareontwikkelaar
country: VS
```

Let op hoe je zowel puntnotatie als bloknotatie kunt gebruiken voor het benaderen en instellen van eigenschappen. Bloknotatie is met name nuttig bij het werken met sleutels die dynamisch worden bepaald of tekens bevatten die niet zijn toegestaan in identificatoren.

## Diepere duik

Associatieve arrays in de vorm van objecten zijn een hoeksteen van JavaScript geweest, en bij uitbreiding Google Apps Script, wat het prototype-gebaseerde erfelijkheidsmechanisme weerspiegelt. In tegenstelling tot talen met traditionele associatieve arrays of woordenboeken (bijvoorbeeld Python's dict), bieden Google Apps Script-objecten een flexibele en krachtige manier om gegevens te structureren, profiterend van de dynamische aard van JavaScript.

Het is echter belangrijk om op te merken dat de ECMAScript 2015-specificatie `Map` en `Set` objecten introduceerde, die een meer rechtlijnige afhandeling van associatieve verzamelingen biedt met bepaalde voordelen ten opzichte van objecten, zoals het behouden van invoegvolgorde en betere prestaties voor grote datasets. Hoewel Google Apps Script deze ook ondersteunt, hangt de keuze tussen het gebruik van objecten of de nieuwere `Map`/`Set` structuren af van specifieke behoeften en prestatieoverwegingen. Voor de meeste taken met associatieve arrays bieden traditionele op objecten gebaseerde implementaties een vertrouwde en veelzijdige benadering, maar het is raadzaam om nieuwere alternatieven te onderzoeken naarmate de complexiteit van je script toeneemt.
