---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:22.698805-07:00
description: "Het maken van een tijdelijk bestand stelt je app in staat om gegevens\
  \ op te slaan die het niet voor altijd nodig heeft. Het komt van pas wanneer je\
  \ te\u2026"
lastmod: '2024-03-13T22:44:51.222192-06:00'
model: gpt-4-0125-preview
summary: Het maken van een tijdelijk bestand stelt je app in staat om gegevens op
  te slaan die het niet voor altijd nodig heeft.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Wat & Waarom?

Het maken van een tijdelijk bestand stelt je app in staat om gegevens op te slaan die het niet voor altijd nodig heeft. Het komt van pas wanneer je te maken hebt met grote gegevensverwerking, caching of wanneer je een kladruimte nodig hebt die automatisch wordt opgeruimd.

## Hoe:

In JavaScript leunen de meeste bewerkingen voor tijdelijke bestanden op externe bibliotheken. Hier is een snel voorbeeld met behulp van de `tmp` bibliotheek, die je kunt installeren met `npm install tmp`.

```javascript
const tmp = require('tmp');

// Een tijdelijk bestand aanmaken
tmp.file((err, path, fd, cleanupCallback) => {
  if (err) throw err;

  console.log(`Bestandspad: ${path}`);
  // Doe dingen met het bestand...

  // Wanneer je klaar bent, ruim het op
  cleanupCallback();
});
```

Een voorbeelduitvoer zou er zo uit kunnen zien:

```
Bestandspad: /tmp/tmp-9Xp2nVn6hB5W.tmp
```

## Diepgaand

Het maken van tijdelijke bestanden heeft een lange geschiedenis in de informatica, teruggaand tot de tijden dat het systeemgeheugen beperkt was en tussenliggende gegevens een plaats nodig hadden om te verblijven. In Node.js kan de `fs` module worden gebruikt om tijdelijke bestanden aan te maken, maar het mist ingebouwde tools voor het genereren van tmp-bestanden.

Het gebruik van bibliotheken zoals `tmp` of `tempfile` is vrij gebruikelijk. Ze creëren unieke bestandsnamen, waardoor het risico van naamconflicten wordt verminderd en ze meestal zelf de opruiming afhandelen. `fs.mkdtemp` kan ook nuttig zijn voor het maken van een tijdelijke directory voor het plaatsen van meerdere tmp-bestanden.

Wat de interne werking betreft, gebruiken deze bibliotheken typisch de native mechanismen van het besturingssysteem om deze bestanden veilig aan te maken, waarbij ze vaak in een door het systeem gedefinieerde tijdelijke directory worden geplaatst. Op Unix-achtige systemen is dit meestal `/tmp`, terwijl Windows iets complexers gebruikt onder `LocalAppData`.

Wanneer je met tijdelijke bestanden omgaat, onthoud dan dat terwijl ze "tijdelijk" zijn, onjuiste afhandeling kan leiden tot beveiligingskwetsbaarheden of achtergebleven bestanden die het systeem vervuilen.

## Zie Ook

- [Node.js fs module](https://nodejs.org/api/fs.html) - voor handmatige bestandsbewerkingen.
- [`tmp` pakket op npm](https://www.npmjs.com/package/tmp) - een hulpprogramma voor tijdelijke bestanden en mappen.
- [`tempfile` pakket op npm](https://www.npmjs.com/package/tempfile) - voor het maken van een willekeurig tijdelijk bestandspad.
