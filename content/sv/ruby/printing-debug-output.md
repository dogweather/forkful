---
title:                "Ruby: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva debug-utskrifter är en viktig del av en programmerares arbetsflöde. Genom att lägga in utskrifter i din kod kan du enkelt identifiera och åtgärda potentiella fel och problem under utvecklingsprocessen. Det är ett effektivt sätt att få en djupare förståelse för hur din kod fungerar och att hitta eventuella buggar.

## Hur man gör

Att skriva ut debug-utskrifter i Ruby är enkelt. Du kan använda `puts` eller `p` för att skriva ut variabler och värden. Till exempel:

```Ruby
# Definiera en variabel
num = 10

# Skriv ut variabelns värde
puts num # Output: 10

# Skriv ut variabelns typ
p num.class # Output: Integer
```

Du kan också använda `pp` för att få bättre formaterade utskrifter eller `inspect` för att få en sträng representation av ett objekt.

```Ruby
# Definiera en hash
hash = { key: "value" }

# Skriv ut hashen med pp
pp hash
# Output: {:key=>"value"}
```

## Djupdykning

Det finns många anledningar till varför det är viktigt att skriva debug-utskrifter i din kod. Först och främst ger det dig möjlighet att testa olika delar av koden och se hur olika värden påverkar dess beteende. Detta är speciellt användbart när du stöter på oväntade resultat eller fel i din kod.

Utöver det kan debug-utskrifter också hjälpa dig att förstå hur dina metoder och funktioner samverkar med varandra. Genom att lägga in utskrifter i början och slutet av en funktion kan du se vilka värden som läses och returneras, vilket kan hjälpa dig att identifiera eventuella buggar.

En annan viktig anledning till att skriva debug-utskrifter är för att förbättra kodens läsbarhet. Genom att inkludera utskrifter kan det bli lättare för andra utvecklare att förstå koden och göra ändringar utan att orsaka oönskade effekter.

## Se även

- [Ruby dokumentation för `puts`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Ruby dokumentation för `p`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-p)
- [Ruby dokumentation för `pp`](https://ruby-doc.org/stdlib-2.7.1/libdoc/pp/rdoc/PP.html)
- [Ruby dokumentation för `inspect`](https://ruby-doc.org/core-2.7.1/Object.html#method-i-inspect)