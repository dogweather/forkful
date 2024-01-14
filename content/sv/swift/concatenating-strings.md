---
title:    "Swift: Sammanslagning av strängar"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Många gånger när man programmerar behöver man kombinera olika strängar för att skapa en längre sträng med önskat innehåll. Detta kan göras genom att använda tekniken för att konkatenera strängar.

## Hur man gör det

För att sätta ihop två strängar i Swift kan man använda sig av operatorn "+" eller metoden "append". Här nedanför finns ett enkelt exempel på dessa två metoder.

```Swift
let förnamn = "Anna"
let efternamn = "Andersson"

// Använda operatorn "+" för att konkatenera strängar
let namn1 = förnamn + efternamn
print(namn1) // Resultat: AnnaAndersson

// Använda metoden "append" för att lägga till strängar till en tom sträng
var namn2 = ""
namn2.append(förnamn)
namn2.append(efternamn)
print(namn2) // Resultat: AnnaAndersson
```

Som du kan se ovan behöver man använda en tom sträng om man vill använda metoden "append". Om man inte sätter förnamnet till en variabel utanför textblocket kan man helt enkelt bara lägga till den direkt i metoden.

```Swift
// På ett enklare sätt
var namn = ""
namn.append("Anna")
namn.append("Andersson")
print(namn) // Resultat: AnnaAndersson
```

Man kan även använda sig av eller kombinera fler olika metoder för att konkatenera strängar. Till exempel kan man använda metoden "joined(separator:)" för att lägga till ett separator-tecken mellan strängarna.

## Fördjupning

När man konkatenerar strängar så skapas en helt ny sträng och de ursprungliga strängarna förlorar sin betydelse. Det är därför viktigt att tänka på prestandan när man konkatenerar flera strängar, särskilt om man hanterar stora datamängder.

En annan viktig faktor att tänka på är korrektheten av konkateneringsoperationen. Om man hanterar olika språk kan det finnas fall där speciella tecken eller diakritiska tecken inte hanteras korrekt på grund av encoding-problem.

## Se även

- [Swift documentation - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hacking with Swift - How to concatenate strings easily](https://www.hackingwithswift.com/articles/141/how-to-concatenate-strings-easily)
- [CodeNugget - How to concatenate strings in Swift](https://codenu.gg/concatenate-strings-swift/)