---
title:    "Swift: Bokstavligen felsökningsutskrift"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Felsökning är en viktig del av att skriva kod, oavsett om du är nybörjare eller erfaren utvecklare. Ett enkelt sätt att inspektera och förstå vad som händer i koden är genom att skriva ut debug-meddelanden. Detta gör det möjligt att spåra buggar och hitta problemområden i koden. 

## Såhär gör du

Att skriva ut debug-output i Swift är enkelt. Du kan använda funktionen `print()` och skicka med variabler eller uttryck som du vill inspektera. Låt oss till exempel säga att du har en variabel `name` som lagrar namnet på en användare. Genom att skriva `print(name)` i din kod kommer du att se värdet av variabeln i konsolen när programmet körs.

```Swift
let name = "Johan"
print(name)
```

Output i konsolen: Johan

Utöver att bara skriva ut värden kan du också kombinera flera variabler eller uttryck genom att använda `String interpolation`. Detta innebär att du kan infoga värden i en textsträng genom att placera dem inom parenteser och förskjutna med ett backslash-tecken. Nedan är ett exempel på hur du kan använda `String interpolation` för att skriva ut värdet av två variabler i en mening:

``` Swift
let num1 = 5
let num2 = 10
print("Summan är \(num1 + num2).")
```

Output i konsolen: Summan är 15.

## Djupdykning

Det finns flera olika sätt att skriva ut debug-output i Swift, beroende på vad du vill uppnå. Du kan även använda funktionen `dump()` för att få en mer detaljerad utskrift av en variabels struktur och alla dess egenskaper. Detta kan vara användbart när du arbetar med komplexa datatyper som arrays eller dictionaries.

Du kan också ändra loggnivån för hur mycket debug-information som ska skrivas ut. Detta görs i Xcode under fliken "Edit Scheme" för ditt projekt. Välj sedan "Run" och navigera till fliken "Arguments". Här kan du lägga till eller ta bort flaggor för att justera loggnivån.

## Se även

- [Officiell Swift-dokumentation om debug-output](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID297)
- [En guide från RayWenderlich om att använda debug-output i Swift](https://www.raywenderlich.com/20646943-swift-debugging-cheat-sheet-getting-quick-answers-in-xcode)
- [En YouTube-video som visar olika sätt att skriva ut debug-output i Swift](https://www.youtube.com/watch?v=FCGrgYM47T8)