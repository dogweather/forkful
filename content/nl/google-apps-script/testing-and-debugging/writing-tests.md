---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:42.016073-07:00
description: "Tests schrijven in Google Apps Script (GAS) betekent het cre\xEBren\
  \ van geautomatiseerde scripts om het gedrag van je codes te verifi\xEBren, zodat\
  \ ze presteren\u2026"
lastmod: '2024-03-13T22:44:50.335436-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven in Google Apps Script (GAS) betekent het cre\xEBren van\
  \ geautomatiseerde scripts om het gedrag van je codes te verifi\xEBren, zodat ze\
  \ presteren\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven in Google Apps Script (GAS) betekent het creëren van geautomatiseerde scripts om het gedrag van je codes te verifiëren, zodat ze presteren zoals verwacht onder verschillende omstandigheden. Programmeurs doen dit om bugs vroegtijdig te ontdekken, de codekwaliteit te verbeteren en updates en onderhoud gemakkelijker te maken.

## Hoe:

Hoewel Google Apps Script geen ingebouwd testraamwerk heeft zoals sommige andere programmeeromgevingen, kun je nog steeds tests schrijven en uitvoeren door eenvoudige GAS-functies te gebruiken of externe testbibliotheken zoals `QUnit` te integreren. Hier is een basisvoorbeeld dat een eenvoudige GAS-functie gebruikt om een andere functie in je script te testen:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test gefaald: add(2, 3) zou 5 moeten zijn, maar was " + result);
  } else {
    Logger.log("Test geslaagd!");
  }
}
```

Het uitvoeren van `testAdd()` logt "Test geslaagd!" als de functie `add` correct werkt of geeft een foutmelding als dit niet het geval is. Voor een geavanceerdere aanpak, het integreren van QUnit met Google Apps Script omvat een paar extra stappen maar biedt een krachtige testomgeving. Een voorbeeld van een QUnit-testopstelling ziet er als volgt uit:

1. Voeg de QUnit-bibliotheek toe aan je project.
2. Maak een test HTML-bestand om de QUnit-tests uit te voeren.
3. Schrijf testgevallen met behulp van QUnit’s syntax.

Hier is een voorbeeld met QUnit:

```javascript
// Voeg QUnit toe door ernaar te linken in een HTML-bestand dat wordt gebruikt om je tests uit te voeren

QUnit.test("Testen van de add-functie", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) zou 5 moeten teruggeven");
});
```

Om de resultaten te zien, open je het HTML-bestand binnen de GAS Script Editor of implementeer je het als een webapp.

## Diepere Duik

Historisch gezien is testen in Google Apps Script enigszins over het hoofd gezien, waarschijnlijk vanwege de oorsprong van het platform en de primaire gebruiksscenario's die zich richten op snelle, kleinschalige automatiseringstaken in plaats van grote applicaties. Als zodanig biedt GAS niet dezelfde robuuste testraamwerken en -tools aan die in meer traditionele programmeeromgevingen te vinden zijn. De gemeenschap heeft zich echter aangepast door open-source bibliotheken te incorporeren en Google's bestaande tools op creatieve wijze te gebruiken.

Het gebruik van bibliotheken zoals QUnit vertegenwoordigt een grote stap voorwaarts, maar komt met zijn eigen set uitdagingen, zoals het opzetten van een geschikte testomgeving en het leren van een extra syntax. Echter, voor degenen die geïnvesteerd zijn in het bouwen van complexere en betrouwbaardere applicaties met GAS, is de inspanning de moeite waard.

Alternatieven zoals het gebruik van eenvoudige GAS-functies voor testen bieden gebruiksgemak en integratie met de GAS-omgeving zonder extra afhankelijkheden, maar missen uitgebreide testfuncties en de mogelijkheid om gemakkelijk te schalen naarmate je project groeit. Tools zoals clasp (de Google Apps Script Command Line Interface) kunnen geavanceerdere workflows, inclusief testen, vergemakkelijken door ontwikkelaars toe te staan te coderen in hun voorkeurs-IDE, wat ruimte biedt voor integratie met externe testraamwerken op een meer naadloze manier.

Conclusie, hoewel GAS misschien niet standaard ondersteuning biedt voor geavanceerde testen, bieden de flexibiliteit van het platform en de innovatieve benaderingen van de gemeenschap haalbare wegen om ervoor te zorgen dat je scripts robuust, betrouwbaar en klaar zijn voor elke taak.
