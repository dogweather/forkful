---
title:    "Fish Shell: Skrivande av tester"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

##Varför
Att skriva tester är en viktig del av programmering eftersom det hjälper till att säkerställa att din kod fungerar på önskat sätt. Detta minskar risken för buggar och fel i din slutliga produkt.

##Så här
För att skriva tester i Fish Shell, använd kommandot "test". Detta tillåter dig att köra olika typer av tester, såsom jämförelser mellan olika värden eller köra kommandon och kontrollera deras utfall.

Exempelvis kan vi skriva ett test för att se om en viss fil existerar i det nuvarande mappen:

```Fish Shell 
test -e filnamn.txt 
```
Denna kod kommer att returnera en sanningsvärdet beroende på om filen finns eller inte. Om filen existerar kommer testet att returnera "true", annars blir resultatet "false".

Du kan också använda "test" kommandot för att jämföra värden. Till exempel, om vi vill testa om en variabel är större än en annan:

```Fish Shell 
test $a -gt 10
```
Denna kod kommer att returnera "true" om variabeln "a" är större än 10, annars blir resultatet "false".

##Djupdykning
För att skriva effektiva tester, är det viktigt att förstå de olika jämförelseoperatorerna som finns tillgängliga i Fish Shell. Här är några av de vanligaste jämförelseoperatorerna du kan använda:

- -eq: lika med
- -ne: inte lika med
- -gt: större än
- -lt: mindre än
- -ge: större än eller lika med
- -le: mindre än eller lika med

Du kan också kombinera flera tester med hjälp av "&&" (och) och "||" (eller) för att skapa mer komplexa tester och ge olika utfall för olika scenarier.

Ett annat tips för att skriva tester är att använda kommentarer för att dokumentera dina tester. Detta kommer att hjälpa dig att förstå vad varje test gör och varför det behövs om du behöver återkomma till dem i framtiden.

##See Also
- [Fish Shell test command documentation](https://fishshell.com/docs/current/commands.html#test)
- [A Beginner's Guide to Writing Tests](https://www.freecodecamp.org/news/writing-tests/) 
- [10 Reasons Why You Should Write Tests](https://medium.com/@searls/10-reasons-why-you-should-write-tests-d41c6b65fd9d)
- [Using Fish Shell for Automated Testing](https://www.linkedin.com/pulse/automated-testing-fish-shell-daniel-holden/)