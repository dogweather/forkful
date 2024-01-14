---
title:                "Gleam: Att skriva tester"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Varför

Att skriva tester är en viktig del av utvecklingsprocessen i alla programmeringsspråk, och det gäller även för Gleam. Genom att skriva tester kan du säkerställa att ditt kod fungerar korrekt och undvika buggar och felaktig funktionalitet.

# Hur man gör

För att skriva tester i Gleam behöver du först importera testbiblioteket genom att lägga till följande rad i din kod:

```Gleam
import gleam/test
```

Sedan kan du skapa en testmodul och lägga till olika testfall med hjälp av `describe` och `test` funktionerna. Här är ett exempel på en testmodul som kontrollerar en funktion som lägger till två tal:

```Gleam
test "additionstest" {
  let result = addition(2, 3)
  
  test.assert_equal(result, 5)
}
```

När du kör testerna kommer du att se en output som visar om testerna har passerat eller misslyckats. Om alla testene passerar får du ett grönt meddelande, annars visas ett rött meddelande.

# Djupdykning

När du skriver tester är det viktigt att täcka alla möjliga scenarion och testa olika typer av input för att verifiera ditt kod. Gleam erbjuder många olika testfunktioner som du kan använda för att skriva omfattande tester för ditt kod. Det är även möjligt att skriva tester för dina egna moduler och funktioner för att säkerställa att de fungerar som förväntat.

# Se även

Här är några användbara länkar för mer information om att skriva tester i Gleam:

- [Gleam testbibliotek] (https://gleam.run/libraries/test/) - officiell dokumentation för testbiblioteket
- [En introduktion till testning i Gleam] (https://mattsparks.se/articles/gleam-testing/) - en praktisk guide till att skriva tester i Gleam
- [Gleam forum] (https://gleam.run/community/) - diskussionsforum för Gleam där du kan ställa frågor och ta del av tips och råd från andra användare.