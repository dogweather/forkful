---
title:    "TypeScript: Läsning av kommandoradsargument"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför Läsa Kommandoradsargument i TypeScript?

Många programmerare stöter på situationer där de behöver interagera med sina program genom att tillhandahålla argument via kommandoraden. Detta kan vara användbart för att ge programmet olika instruktioner eller data för att kunna anpassa dess beteende. I denna bloggpost kommer vi att gå igenom hur man kan läsa kommandoradsargument i TypeScript och hur det kan användas i dina projekt.

## Så här gör du:

För att läsa kommandoradsargument i TypeScript behöver du använda Node.js inbyggda `process` objekt. Först måste du importera detta objekt i din kod genom att skriva `import process from 'process';`. Sedan kan du använda `process.argv` för att få en array av alla kommandoradsargument som tillhandahålls när du kör ditt program.

Låt oss titta på ett enkelt exempel på hur man kan använda detta i praktiken:

```TypeScript
// Importera process-objektet
import process from 'process';

// Hämta argumenten från process.argv
const args = process.argv;

// Ta ut första argumentet efter att ha ignorerat den första (vilket är sökvägen till din körafil)
const arg1 = args[2];

// Skriv ut argumentet
console.log(arg1);
```

Om vi nu kör detta program med argumentet "Hello" (`node index.ts Hello`), kommer outputen att bli "Hello".

Vi kan även lägga till fler kommandoradsargument och hämta dem genom att använda det specifika indexet i arrayen. Till exempel:

```TypeScript
// Importera process-objektet
import process from 'process';

// Hämta argumenten från process.argv
const args = process.argv;

// Ta ut andra argumentet efter att ha ignorerat de två första (sökvägen till filen och första argumentet)
const arg2 = args[3];

// Skriv ut argumentet
console.log(arg2);
```

Om vi nu kör detta program med argumenten "Hello" och "World" (`node index.ts Hello World`), kommer outputen att bli "World".

## Djupdykning:

I våra exempel har vi bara tagit ut och skrivit ut ett enkelt argument åt gången, men du kan också använda andra metoder och tekniker för att hantera kommandoradsargumenten. En vanlig metod är att använda alternativ (flags) som specificerar vissa handlingar eller parametrar för ditt program.

Till exempel kan vi lägga till en alternativ flagga `-g` som står för "greet" och skriver ut en personlig hälsning meddelande när den används i kommandoraden. Vi kan också lägga till ett värde för denna flagga som representerar vilken person som ska hälsas på. Detta kan se ut så här:

```TypeScript
// Importera process-objektet
import process from 'process';

// Hämta argumenten från process.argv
const args = process.argv;

// Ta ut värdena för flaggan -g och -n
const greetFlag = args.indexOf('-g'); // Indexet för flaggan
const name = args[greetFlag + 1]; // Värdet som kommer efter flaggan

// Om flaggan finns
if (greetFlag !== -1) {
  // Skriv ut personlig hälsning
  console.log(`Hello ${name}! Welcome to our program.`);
}
```

Nu kan vi köra programmet med flaggan och ett värde (`node index.ts -g John`) och outputen blir "Hello John! Welcome to our program."

## Se även:

Här är några länkar för mer information om hur man kan läsa och hantera kommandoradsargument i TypeScript:

- [Officiell dokumentation för processobjektet i Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Bloggpost om interagera med kommandoradsargument i Typescript](https://www.grapecity.com/en/blogs/command-line-args-typescript)
- [Youtube tutorial om kommandoradsargument i Node.js med TypeScript](https://www.youtube.com/watch?v=c828j0tceNE)

Med kunskapen om hur man arbetar med kommand