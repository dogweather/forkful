---
title:    "Ruby: Extrahering av delsträngar"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt behöver vi ibland dela upp en sträng i mindre delar. Detta kan vara för att hitta ett specifikt tecken, ord eller för att bara dela upp en lång sträng i mer hanterbara delar. För att kunna göra detta behöver vi kunna extrahera substrängar från en större sträng. Detta är där Ruby kommer in i bilden och erbjuder en enkel och effektiv metod för att göra detta.

## Så här gör du

För att extrahera en substräng från en större sträng i Ruby, kan vi använda metoden `[]` eller `slice()`. Låt oss börja med ett enkelt exempel där vi har en sträng med en lista över namn separerade med kommatecken.

```Ruby
namn = "Emma,Peter,Lisa,Anna"
```

Om vi nu vill extrahera namnen en efter en, kan vi använda metoden `split()`, som kommer att dela upp strängen och returnera en array med varje namn som ett element.

```Ruby
namn_lista = namn.split(",")

# Output: ["Emma", "Peter", "Lisa", "Anna"]
```

För att sedan extrahera ett specifikt namn från listan, kan vi använda indexpositionen för det namnet vi vill ha.

```Ruby
namn_lista[2]

# Output: "Lisa"
```

Vi kan också använda en `range` för att extrahera flera namn från listan samtidigt.

```Ruby
namn_lista[1..3]

# Output: ["Peter", "Lisa", "Anna"]
```

Om vi vill extrahera en del av en sträng, kan vi också använda metoden `[]` eller `slice()` med start- och slutpositioner för det område av strängen vi vill ha.

```Ruby
namn[5..9]

# Output: "Peter"
```

## Djupdykning

Det finns många andra sätt att extrahera substrängar i Ruby, inklusive användning av regular expressions och andra inbyggda metoder som `scan()` och `match()`. Det är också viktigt att vara medveten om att det finns olika sätt att utföra samma sak, men det är alltid bra att välja det enklaste och mest lättlästa alternativet.

## Se även

- [String Class](https://ruby-doc.org/core-3.0.2/String.html)
- [Arrays in Ruby](https://ruby-doc.org/core-3.0.2/Array.html)
- [Regular Expressions in Ruby](https://ruby-doc.org/core-3.0.2/Regexp.html)