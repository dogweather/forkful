---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:51.382058-07:00
description: "YAML, dat staat voor \"YAML Ain't Markup Language\", is een voor mensen\
  \ leesbare gegevensserialisatiestandaard die vaak wordt gebruikt voor\u2026"
lastmod: '2024-03-13T22:44:50.353968-06:00'
model: gpt-4-0125-preview
summary: YAML, dat staat voor "YAML Ain't Markup Language", is een voor mensen leesbare
  gegevensserialisatiestandaard die vaak wordt gebruikt voor configuratiebestanden
  en gegevensuitwisseling tussen talen met verschillende gegevensstructuren.
title: Werken met YAML
weight: 41
---

## Wat & Waarom?

YAML, dat staat voor "YAML Ain't Markup Language", is een voor mensen leesbare gegevensserialisatiestandaard die vaak wordt gebruikt voor configuratiebestanden en gegevensuitwisseling tussen talen met verschillende gegevensstructuren. Programmeurs werken vaak met YAML vanwege de eenvoud en leesbaarheid, vooral in projecten die uitgebreide configuratie vereisen of bij het overbrengen van gestructureerde gegevens tussen verschillende systemen.

## Hoe te:

Hoewel Google Apps Script (GAS) geen YAML-analyse of -serialisatie native ondersteunt, kunt u YAML-gegevens manipuleren door JavaScript-bibliotheken te gebruiken of aangepaste analysefuncties te schrijven. Laten we voor de demonstratie eens kijken hoe je een YAML-string kunt ontleden met een aangepaste functie, aangezien externe bibliotheken niet rechtstreeks kunnen worden geïmporteerd in GAS.

Stel dat u een eenvoudige YAML-configuratie heeft:

```yaml
titel: YAML-voorbeeld
beschrijving: Een voorbeeld van hoe om te gaan met YAML in Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuratie
```

Om dit in Google Apps Script te ontleden, gebruik je de mogelijkheden voor het manipuleren van strings in JavaScript:

```javascript
function ontledenYAML(yamlString) {
  var resultaat = {};
  var regels = yamlString.split("\n");
  for (var i = 0; i < regels.length; i++) {
    var regel = regels[i];
    if (regel.includes(":")) {
      var delen = regel.split(":");
      var sleutel = delen[0].trim();
      var waarde = delen[1].trim();
      // Basisafhandeling voor arrays
      if (waarde.startsWith("-")) {
        waarde = [waarde.substring(1).trim()];
        while (i + 1 < regels.length && regels[i + 1].trim().startsWith("-")) {
          i++;
          waarde.push(regels[i].trim().substring(1).trim());
        }
      }
      resultaat[sleutel] = waarde;
    }
  }
  return resultaat;
}

function testYamlOntleden() {
  var yaml = "titel: YAML-voorbeeld\nbeschrijving: Een voorbeeld van hoe om te gaan met YAML in Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuratie";
  var ontleden = ontledenYAML(yaml);
  Logger.log(ontleden);
}
```

Wanneer `testYamlOntleden()` wordt uitgevoerd, wordt het volgende uitgevoerd:

```
{ titel: 'YAML-voorbeeld',
  beschrijving: 'Een voorbeeld van hoe om te gaan met YAML in Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuratie' ] }
```

Deze aangepaste ontledingsaanpak is vrij basis en moet mogelijk worden aangepast om complexe YAML-bestanden te kunnen hanteren.

## Diepgaande duik

YAML, voor het eerst uitgebracht in 2001, was bedoeld om leesbaarder te zijn dan zijn voorgangers zoals XML of JSON. Hoewel de eenvoud en gebruiksgemak breed worden gewaardeerd, presenteert het omgaan met YAML in Google Apps Script uitdagingen vanwege het ontbreken van directe ondersteuning. Daarom vertrouwen programmeurs vaak op de veelzijdigheid van JavaScript om YAML-gegevens te ontleden en te genereren. Echter, voor complexe gebruiksscenario's, met name die met diepe nesten en geavanceerde gegevensstructuren, kan deze methode omslachtig en foutgevoelig zijn.

JSON daarentegen wordt native ondersteund in Google Apps Script en de meeste andere programmeeromgevingen, en biedt een eenvoudiger aanpak voor gegevensserialisatie en -deserialisatie zonder extra analyse-overhead. De syntax van JSON is minder breedsprakig dan die van YAML, waardoor het geschikter is voor gegevensuitwisseling in webtoepassingen. Desalniettemin blijft YAML populair voor configuratiebestanden en situaties waarin menselijke leesbaarheid van het grootste belang is.

Bij het werken met YAML in Google Apps Script, overweeg de afwegingen tussen leesbaarheid en gebruiksgemak. Voor uitgebreide YAML-manipulatie kan het de moeite waard zijn om externe tools of diensten te verkennen die YAML kunnen omzetten naar JSON voordat het binnen uw script wordt verwerkt.
