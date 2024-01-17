---
title:                "Skriva tester"
html_title:           "C++: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & varför?
Vad är testning och varför borde programmerare göra det? Testning är en process där man kontrollerar att ens kod fungerar som det ska och uppfyller de önskade kraven. Det är viktigt för att säkerställa att programmet är pålitligt och fungerar som det ska när det används av användare. Det är också ett sätt att upptäcka och åtgärda fel i koden innan de når slutanvändaren.

## Hur man gör det:
Ett grundläggande sätt att testa sin kod är att använda sig av "assertions". Det innebär att man skriver kod som kontrollerar att ett uttryck är sant, annars kastas ett felmeddelande. Här är ett exempel på en assertion i C++:

```C++
assert(2+2 == 4);
```

Om uttrycket är sant kommer inget att hända, men om det är falskt kommer det att kasta ett felmeddelande. Det är ett enkelt sätt att kontrollera att ens förväntningar om vad koden ska göra stämmer överens med vad som faktiskt händer.

## Djupdykning:
Testning har alltid varit en viktig del av programmering, men i takt med att programmen blir allt mer komplext och används i större skala, blir testning ännu viktigare. Ett annat sätt att testa sin kod är genom att använda sig av testramverk som till exempel Google Test eller Catch2. Dessa ramverk ger fler möjligheter för att skriva och köra tester och ger en mer strukturerad och organiserad approach.

Ett annat alternativ är "Test Driven Development" (TDD), där man skriver testkod innan man skriver själva produktionskoden. Detta hjälper till att fokusera på vad koden ska åstadkomma och skapar även en säkerhetsnet för att upptäcka eventuella problem senare.

## Se även:
- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)
- [Test Driven Development för nybörjare](https://youtu.be/q1xob-rDD2g)