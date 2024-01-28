---
title:                "Refaktorisering"
date:                  2024-01-26T01:18:47.247381-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omarbeta din kod för att göra den renare, mer underhållbar utan att ändra dess externa beteende. Programmerare refaktoriserar för att förbättra läsbarheten, minska komplexiteten och göra kodbasen mer benägen för framtida uppdateringar eller funktionstillägg.

## Hur man gör:
Låt oss säga att du har en del kod där du gör några upprepade beräkningar eller strängmanipulationer över flera funktioner. Det är ett huvudmål för refaktorisering. Här är ett före-och-efter exempel med Gleam, som betonar starkt typsäkerhet och oföränderlighet:

```gleam
// Före refaktorisering
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("Ytan är \(area)")
}

// Efter refaktorisering
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("Ytan är \(area)")
}

// I en annan del av din kod, kommer du anropa print_area så här:
print_area(calculate_area(10, 20))
```

Exempelutskrift:
```
Ytan är 200
```

Genom refaktorisering har vi gjort `print_area` mer fokuserad på att bara skriva ut, medan beräkningen hanteras på annat håll, vilket gör koden mer modulär och lättare att återanvända eller testa.

## Djupdykning
Refaktorisering, som ett koncept, har funnits lika länge som programmering själv—att återbesöka och städa upp i koden är en del av god hushållning. Den moderna formaliseringen av refaktorisering, tillsammans med många av de tekniker och mönster som används idag, kan spåras tillbaka till Martin Fowlers banbrytande bok "Refactoring: Improving the Design of Existing Code" som publicerades 1999.

I Gleam-ekosystemet har refaktorisering specifika överväganden. Ett av de mest betydande är den starka typkontrollen vid kompileringstid, som kan hjälpa till att upptäcka misstag tidigt när du flyttar saker och ting. Gleams mönsterpassning och oföränderlighetsfunktioner kan också vägleda dig till att skriva klarare, mer koncis kod—ett av de primära målen med refaktorisering.

Alternativ till refaktorisering kan inkludera att skriva om kod från grunden eller lappa kod med snabba fixar. Refaktorisering är dock vanligtvis det säkraste och mest effektiva sättet att förbättra befintlig kod utan att introducera nya buggar, eftersom det innebär inkrementella, väl understrukna, beteendepreserverande transformationer.

## Se också
- Martin Fowlers bok "Refactoring": https://martinfowler.com/books/refactoring.html
- Gleam-språkets webbplats, med ytterligare dokumentation och exempel: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" av Martin Fowler (för underliggande principer tillämpliga över språk): https://martinfowler.com/books/refactoring.html
