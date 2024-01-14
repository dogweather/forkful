---
title:    "Gleam: Skriva tester"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmeringsprocessen. Genom att skriva tester kan du säkerställa att din kod fungerar som den ska och upptäcka eventuella buggar eller felaktigheter innan de når produktion.

## Så här

För att skriva tester i Gleam behöver du använda modulen `gleam/test`. Inom denna modul finns det olika funktioner som hjälper dig att skapa tester och verifiera resultat. Här är ett exempel på hur du kan skriva en test för en funktion som adderar två tal:

```Gleam
fn test_add() {
  assert.equal(5, add(2, 3))
}

fn add(x, y) { x + y }

```

När du kör denna test kommer du att få ett meddelande som bekräftar att testet har lyckats eller ett felmeddelande om testet har misslyckats.

## Djupdykning

Att skriva tester är en del av testdriven utveckling (TDD) metodiken. När du använder TDD, skriver du tester först och sedan kodar du för att uppfylla testerna. Detta hjälper dig att skapa en välstrukturerad och testbar kod.

En annan viktig del av att skriva tester är att täcka olika scenarier och gränsvärden. Genom att testa för olika situationer kan du säkerställa att din kod fungerar för alla möjliga fall.

## Se också

- [Gleam test modul](https://gleam.run/docs/test/)
- [Testdriven utveckling – en introduktion](https://www.agical.se/utbildning/tdd/)