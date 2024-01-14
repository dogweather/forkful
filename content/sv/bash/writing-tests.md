---
title:                "Bash: Skriva tester"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-tests.md"
---

{{< edit_this_page >}}

##Varför

I dagens snabbrörliga och krävande programmeringsvärld är det viktigt att ha en välstrukturerad och pålitlig kod. En viktig del av detta är att skriva tester för att säkerställa att koden fungerar som den ska. Läs vidare för att ta reda på varför det är viktigt att skriva tester i Bash-programmering och hur man gör det.

##Så här gör du

När det kommer till att skriva tester i Bash-programmering finns det två huvudsakliga sätt att göra det: manuellt och automatiskt. Ett manuellt test är när du som programmerare kör din kod och kontrollerar att den arbetar korrekt. Detta kan vara väldigt tidskrävande och ineffektivt, särskilt när koden blir mer komplex. Istället föreslår vi att du använder dig av automatiska tester som körs av en testsvit.

För att skriva en automatisk testsvit i Bash måste du först skapa en fil med all kod som ska testas. Låt oss säga att du har en funktion som heter "add_numbers" som tar två tal som argument och returnerar deras summa. Du skulle då skapa en fil "add_numbers.sh" och skriva följande kod:

```Bash
#!/bin/bash

add_numbers() {
    result=$(( $1 + $2 ))
    echo $result
}

```

Därefter behöver du skapa en fil specifikt för dina tester, till exempel "test_add_numbers.sh". I denna fil kan du skriva en testsvit som ser ut så här:

```Bash
#!/bin/bash

# Kontrollerar om "add_numbers" returnerar rätt summa för två positiva tal
expected_result=7
actual_result=$(bash add_numbers.sh 3 4)

if [ $expected_result -eq $actual_result ]
then
    echo "Test Passed"
else
    echo "Test Failed"
fi
```

Som du kan se definierar vi först det förväntade resultatet, som i detta fall är summan av 3 och 4 (7). Sedan kör vi funktionen och sparar resultatet i en variabel. Slutligen jämför vi det förväntade resultatet med det faktiska och skriver ut om testet passerades eller misslyckades.

Du kan också skapa fler tester för att täcka olika scenarier, såsom negativa tal eller felaktiga datatyper som argument. Detta ger dig en grundligare och pålitligare testsvit för din kod.

##Dyk ner i detaljerna

Skrivandet av tester i Bash kan verka komplicerat, särskilt om du är ny till programmering. Men med lite övning och efter att ha förstått syntaxen blir det snabbt en naturlig och viktig del av din arbetsprocess. Ett annat tips är att söka efter tutorials och guider på nätet för att få en bättre förståelse för hur man skriver effektiva tester.

En annan viktig aspekt av att skriva tester är att göra det till en vanlig del av din programmeringsrutin. Testsuiter bör köras efter varje ändring i koden för att snabbt upptäcka eventuella buggar som kan påverka funktionaliteten. Detta sparar tid och energi i det långa loppet.

##Se också

- [Bash Guide](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial: Writing Bash Tests](https://erikanapoletano.com/tutorial-writing-tests-for-your-bash-scripts/)
- [Introduction to Test Driven Development (TDD)](https://medium.freecodecamp.org/test-driven-development-what-it-is-and-what-it-is-not-41fa6bca02a2)