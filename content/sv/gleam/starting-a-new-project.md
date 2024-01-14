---
title:    "Gleam: Att påbörja ett nytt projekt"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt är alltid spännande och kan vara en rolig utmaning. Det ger dig möjlighet att utforska nya språk och koncept, och skapa något unikt som du kan vara stolt över.

## Så här gör du

För att komma igång med Gleam behöver du först installera det på din dator. Sedan kan du börja skriva din kod i en textredigerare som stödjer Språket. När du har skrivit och sparat din kod, kan du kompilera den med hjälp av kommandot "gleam build". Detta kommer att skapa en excutable fil som du sedan kan köra och se resultatet av din kod.

```Gleam
fn main() {
  let namn = "Välkommen till Gleam bloggen!"
  let språk = "Det här är ett exempel på hur du använder code blocks i Gleam"
  io.print(namn)
  io.print(språk)
}
```

Output:

Välkommen till Gleam bloggen!
Det här är ett exempel på hur du använder code blocks i Gleam

## Djupdykning

När du börjar ett nytt projekt i Gleam, är det viktigt att du förstår grundläggande koncept som funktioner, variabler och datastrukturer. Dessutom måste du vara bekant med syntaxen i Språket och hur du bygger upp ditt program genom att använda moduler och filstrukturer.

För att bli ännu mer framgångsrik i Gleam, rekommenderar vi att du också läser dokumentationen och engagerar dig i communityt. Där kan du få hjälp, ställa frågor och lära dig av andra Gleam-utvecklare.

## Se även

- Gleam språk: https://gleam.run/
- Gleam dokumentation: https://gleam.run/documentation/
- Gleam community: https://gleam.run/community/
- VS Code utökning för Gleam: https://marketplace.visualstudio.com/items?itemName=gleam-vm.vscode-gleam

Tveka inte att utforska Gleam och skapa något fantastiskt med det! Lycka till med ditt nya projekt!