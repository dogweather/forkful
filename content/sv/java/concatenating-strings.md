---
title:                "Sammanslagning av strängar"
html_title:           "Java: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Det finns olika anledningar till varför man skulle vilja använda sig av strängkonkatinering i Java. Det kan exempelvis vara för att skapa mer dynamiska meddelanden eller för att bygga upp en längre sträng utifrån flera mindre delar. I denna artikel kommer vi att gå igenom hur man kan använda sig av strängkonkatinering i Java på ett enkelt och effektivt sätt.

## Hur man gör

För att konkatinera strängar i Java används "+" operatorn. Vi kan konkatinera flera strängar på ett enkelt sätt genom att sätta "+" mellan varje sträng. Se exempel nedan:

```Java
String name = "John";
String lastName = "Doe";
String fullName = name + " " + lastName;

System.out.println(fullName);
```

Output:
```John Doe```

I detta exempel sätts ett mellanslag mellan namn och efternamn genom att inkludera en mellanslagssträng mellan variablerna ```name``` och ```lastName```.

En annan metod för strängkonkatinering är genom att använda metoden ```concat()```. Denna metod tar en sträng som parameter och konkatineras med den befintliga strängen. Se exempel nedan:

```Java
String greeting = "Hej ";
String name = "Sofia";

System.out.println(greeting.concat(name));
```

Output:
```Hej Sofia```

Det är även möjligt att konkatinera en sträng med andra datatyper, så som heltal eller decimaltal. I sådana fall så kommer Java automatiskt att konvertera datatyperna till strängar innan konkatineringsprocessen utförs.

## Djupdykning

När man konkatinerar strängar i Java skapar man en ny sträng varje gång. Detta betyder att de ursprungliga strängarna inte förändras utan en ny sträng skapas istället. Det är viktigt att tänka på när man arbetar med stora och många strängar för att undvika onödig minnesanvändning.

En annan viktig aspekt att tänka på är att strängar i Java är omuterbara, vilket betyder att de inte kan ändras efter att de har skapats. Detta betyder att varje gång en ändring görs på en befintlig sträng, så skapas en ny sträng istället. Detta kan påverka prestandan om man konkatinerar strängar i en loop, eftersom det skapar en stor mängd onödiga strängar.

## Se även

Här är några användbara länkar som kan hjälpa dig att lära dig mer om strängkonkatinering i Java:

- [Java String Concatenation Tutorial](https://www.javatpoint.com/java-string-concatenation)
- [Official Java Documentation on String Concatenation](https://docs.oracle.com/javase/tutorial/java/data/converting.html)
- [Oracle's Guide to String Concatenation in Java](https://www.oracle.com/technical-resources/articles/java/juneau-string-optimization.html)