---
title:                "Logboekregistratie"
aliases:
- nl/google-apps-script/logging.md
date:                  2024-02-01T21:55:59.373288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/google-apps-script/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen in programmeren houdt in dat gebeurtenissen, fouten of opmerkelijke gebeurtenissen tijdens de uitvoering worden geregistreerd. Programmeurs doen dit om problemen te debuggen, prestaties te monitoren en een registratie van operationele gegevens bij te houden, waardoor het cruciaal is voor het onderhouden en begrijpen van het gedrag van software in productie.

## Hoe te:

In Google Apps Script kan loggen worden uitgevoerd met behulp van verschillende methoden, zoals de `Logger`-klasse en `console.log()`. De Logger-klasse is de traditionele manier, geschikt voor eenvoudige debugging en ontwikkelingsdoeleinden. Sinds recente updates biedt `console.log()` meer flexibiliteit en integratie met Stackdriver Logging, wat een robuustere oplossing biedt voor het monitoren van uw Apps Scripts op Google Cloud Platform.

**Gebruikmakend van Logger:**

```javascript
function logSample() {
  Logger.log('Dit is een eenvoudig logbericht');
  
  var value = 5;
  Logger.log('De waarde is: %s', value); // Stringformattering
}

// Om het log te bekijken:
// 1. Voer de logSample-functie uit.
// 2. Bekijk -> Logs
```

**Voorbeeld Logger-uitvoer:**

```
[22-04-20 10:00:00:000 PDT] Dit is een eenvoudig logbericht
[22-04-20 10:00:00:001 PDT] De waarde is: 5
```

**Gebruikmakend van console.log():**

```javascript
function consoleLogSample() {
  console.log('Dit bericht gaat naar Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Een object loggen:', obj);
}

// Logs kunnen worden bekeken in de Google Cloud Platform (GCP) console onder Stackdriver Logging
```

**Voorbeeld console.log() Uitvoer:**

```
Dit bericht gaat naar Stackdriver Logging
Een object loggen: {name: "Jane", role: "Developer"}
```

Door over te schakelen naar `console.log()` voor complexe applicaties, kunnen ontwikkelaars efficiënt logs parsen en analyseren met behulp van de krachtige filters en tools die GCP biedt, wat niet zo eenvoudig is met de traditionele Logger-klasse.

## Diepgaande duik:

Loggen in Google Apps Script is aanzienlijk geëvolueerd. Aanvankelijk was de `Logger`-klasse de primaire methode voor ontwikkelaars om hun scripts te debuggen. Het is eenvoudig en voldoende voor basis scripts, maar het mist de mogelijkheden die nodig zijn voor moderne cloudapplicaties, zoals het doorzoeken van logs of het analyseren van logtrends in de loop van de tijd.

De introductie van `console.log()` overbrugde deze kloof door Google Apps Script-logboekregistratie te integreren met Google Cloud's Stackdriver Logging (nu Operations Suite genoemd), en bood een gecentraliseerd platform voor logboekregistratie, monitoring en debugging van applicaties. Dit maakte niet alleen logboekregistratie op schaal mogelijk, maar opende ook geavanceerde functies voor logbeheer, zoals loggebaseerde metingen, realtime loganalyse en integratie met andere Google Cloud-diensten.

Hoewel `Logger` nog steeds een doel dient voor snelle debugging en loggen in kleinere scripts, weerspiegelt de evolutie naar het gebruik van `console.log()` een bredere verschuiving in de ontwikkeling van schaalbare, cloud-native applicaties. Het benadrukt de toewijding van Google om ontwikkelaars te voorzien van tools die tegemoetkomen aan de complexiteit en schaal van de applicaties van vandaag. Nieuwkomers moeten echter bewust zijn van de ietwat steilere leercurve en de noodzaak om vertrouwd te raken met de concepten van Google Cloud Platform. Desondanks is de verhuizing voordelig voor ontwikkelaars die de mogelijkheden van de cloud volledig willen benutten. Deze afstemming met cloudservices maakt deel uit van een bredere trend in softwareontwikkeling, die het belang van robuuste, schaalbare logmechanismen in het tijdperk van cloud computing benadrukt.
