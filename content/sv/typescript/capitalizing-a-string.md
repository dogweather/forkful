---
title:    "TypeScript: Stora bokstäver i en sträng"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Varför du behöver koda i TypeScript: Att göra en string med stort begynnelsebokstav

Du kanske har hört talas om att "kapitalisera en sträng", men vad är egentligen syftet med det? Och hur kan TypeScript hjälpa dig att göra det på ett enkelt sätt? Läs vidare för att ta reda på det!

## Hur man gör det

Först måste vi förstå vad det betyder att "kapitalisera en sträng". I grund och botten innebär det att du tar en sträng med små bokstäver och omvandlar den så att första bokstaven blir en stor bokstav. Till exempel, om vi har strängen "hej", så vill vi omvandla den till "Hej". I TypeScript kan vi enkelt åstadkomma detta med hjälp av den inbyggda metoden `toUpperCase()` på strängar.

Låt oss skriva ett enkelt exempel:

```TypeScript
let sträng = "hej";
console.log(sträng.toUpperCase());
```

Det här kommer att skriva ut "Hej" i konsolen. Vi kan också använda TypeScript-typen "string" istället för att deklarera variabeln som `let`:

```TypeScript
let sträng: string = "hej";
console.log(sträng.toUpperCase());
```

Det är enkelt, eller hur? TypeScript gör det enkelt att använda inbyggda metoder som `toUpperCase()` för att manipulera strängar på ett effektivt sätt.

## En djupare dykning

Nu när vi förstår hur man kapitaliserar en sträng, låt oss titta på några olika sätt som vi kan använda det på i vår kod. Först och främst kan vi använda det för att göra vår kod mer läsbar och konsekvent. Till exempel kan du använda det när du skriver ut användarnamn eller annan personlig information för att se till att den alltid visas på samma sätt.

Vi kan också kombinera det med andra metoder för att göra mer avancerade manipulationer av strängar. Till exempel, för att skapa ett användarnamn som endast innehåller en stor första bokstav av varje ord i en persons namn, kan vi använda följande kod:

```TypeScript
let förnamn = "anna";
let efternamn = "andersson";
let användarnamn = `${förnamn.toUpperCase().charAt(0)}${efternamn.toUpperCase()}`;
console.log(användarnamn); // Resultatet blir "AANDERSSON"
```

Som ni kan se använder vi `toUpperCase()` för att omvandla både förnamn och efternamn till stora bokstäver. Sedan tar vi bara den första bokstaven av förnamnet och kombinerar den med efternamnet för att skapa ett unikt användarnamn.

## Se även

- [JavaScript substring() metod för att extrahera delar av strängar](https://www.w3schools.com/jsref/jsref_substring.asp)
- [TypeScript-strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)