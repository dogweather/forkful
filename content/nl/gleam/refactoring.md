---
title:                "Refactoring"
date:                  2024-01-28T22:05:54.456237-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herwerken van je code om het schoner, beter onderhoudbaar te maken, zonder het externe gedrag te veranderen. Programmeurs refactoren om de leesbaarheid te verbeteren, complexiteit te verminderen, en om de codebasis beter geschikt te maken voor toekomstige updates of functie-toevoegingen.

## Hoe:
Stel je hebt een stuk code waar je enkele berekeningen of stringmanipulaties herhaaldelijk uitvoert over meerdere functies. Dat is een prima doelwit voor refactoring. Hier is een voor-en-na met Gleam, dat een sterke nadruk legt op typeveiligheid en onveranderlijkheid:

```gleam
// Voor refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  laat area = calculate_area(width, height)
  io.println("De oppervlakte is \(area)")
}

// Na refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("De oppervlakte is \(area)")
}

// In een ander deel van je code, roep je print_area zo aan:
print_area(calculate_area(10, 20))
```

Voorbeelduitvoer:
```
De oppervlakte is 200
```

Door te refactoren, hebben we `print_area` meer gefocust gemaakt op enkel het printen, terwijl de berekening elders wordt behandeld, wat de code modulairder en gemakkelijker te hergebruiken of te testen maakt.

## Diepe Duik
Refactoring, als concept, bestaat al zolang als programmeren zelf—terugkeren en code opruimen is deel van goed huishouden. De moderne formalisering van refactoring, samen met vele van de technieken en patronen die vandaag gebruikt worden, kunnen herleid worden naar het baanbrekende boek van Martin Fowler "Refactoring: Improving the Design of Existing Code", gepubliceerd in 1999.

In het Gleam ecosysteem heeft refactoring specifieke overwegingen. Een van de belangrijkste is de sterke typecontrole bij het compileren, wat kan helpen fouten vroegtijdig te ontdekken als je dingen verplaatst. Gleam's patroonmatching en onveranderlijkheidsfuncties kunnen je ook leiden naar het schrijven van duidelijkere, beknoptere code—een van de primaire doelen van refactoring.

Alternatieven voor refactoring kunnen het herschrijven van code vanaf nul of het patchen van code met snelle oplossingen omvatten. Refactoring is echter meestal de veiligste en meest efficiënte benadering om bestaande code te verbeteren zonder nieuwe bugs te introduceren, aangezien het incrementele, goed onderbouwde, gedragsbehoudende transformaties omvat.

## Zie Ook
- Het boek "Refactoring" van Martin Fowler: https://martinfowler.com/books/refactoring.html
- De Gleam-taalwebsite, met aanvullende documentatie en voorbeelden: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" van Martin Fowler (voor de onderliggende principes toepasbaar over talen): https://martinfowler.com/books/refactoring.html
