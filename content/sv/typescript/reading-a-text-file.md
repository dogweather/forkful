---
title:    "TypeScript: Att läsa en textfil"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

##Varför

Innan vi går in på hur man läser en textfil med TypeScript, är det viktigt att förstå varför det kan vara en användbar kunskap att ha. Att kunna läsa och bearbeta textfiler är en grundläggande förmåga inom programmering, och det kan vara till nytta i många olika situationer. Till exempel kan det vara användbart när man behöver hantera stora mängder data eller vill skapa en visualisering baserad på en textfil.

##Så här gör du

För att läsa en textfil med TypeScript behöver du först och främst skapa en instans av en File Reader. Detta görs genom att definiera en variabel av typen FileReader och sedan kalla på konstruktorn för att skapa en ny instans av klassen.

```TypeScript
let reader: FileReader = new FileReader();
```

Sedan behöver du ange vilken textfil du vill läsa genom att använda metoden `readAsText()`. Denna metod tar emot två parametrar - filen som ska läsas och encoding-typen som används. I detta exempel använder vi encoding-typen "UTF-8".

```TypeScript
reader.readAsText(file, "UTF-8");
```

Nästa steg är att definiera en callback-funktion som kommer att anropas när textfilen har lästs in helt. Detta gör vi genom att använda eventet `onload`, och sedan använda `reader.result` för att få tillgång till den inlästa texten. I exemplet nedan visar vi hur man skriver ut den inlästa texten i konsolen.

```TypeScript
reader.onload = (event) => {
  console.log(reader.result);
}
```

Slutligen behöver vi lägga till en try/catch-block för att hantera eventuella fel som kan uppstå under läsningen av textfilen. Om läsningen är framgångsrik kommer `reader.result` att innehålla hela textfilens innehåll.

```TypeScript
try {
  let file: File = // Hämta textfilen från en input-element eller server
  reader.readAsText(file, "UTF-8");
} catch(e) {
  console.log("Det gick inte att läsa textfilen: " + e.message);
}
```

##Djupdykning

Att läsa en textfil med TypeScript kan verka enkelt, men det finns faktiskt ett antal saker som är viktiga att hålla koll på. En av de vanligaste hindren är att textfiler kan ha olika encoding-typer, och det är därför viktigt att välja rätt encoding-typ när man läser en fil. Annars riskerar man att få felaktig eller korrupt data.

Det finns också flera olika sätt att läsa en textfil på, beroende på vilken typ av innehåll som ska behandlas. Man kan till exempel använda metoden `readAsBinaryString()` om man läser en binär fil, eller `readAsDataURL()` om man vill läsa en fil som ska användas för att skapa en data-URL.

En annan viktig aspekt är att hantera stora textfiler. Om filen du försöker läsa är mycket stor kan det leda till minnesproblem och programmet kan krascha. I dessa fall kan det vara användbart att använda sig av FileReaderSync-objektet, vilket möjliggör synkron läsning istället för asynkron läsning.

##Se också

- [FileReader API reference](https://developer.mozilla.org/en-US/docs/Web/API/FileReader) (engelska)
- [Textfilens encoding-specifikationer](https://en.wikipedia.org/wiki/Character_encoding) (engelska)
- [En fullständig guide för att läsa stora textfiler i JavaScript](https://stackabuse.com/reading-a-file-line-by-line-in-node-js/) (engelska)