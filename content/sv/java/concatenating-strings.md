---
title:    "Java: Sammanslående strängar"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Den här bloggposten handlar om att sammanfoga strängar i Java, en grundläggande men viktig del av programmering. Genom att lära sig hur man sammanfogar strängar kan du skapa mer dynamiska och användarvänliga program.

## Hur man gör
Det finns flera sätt att sammanfoga strängar i Java, men det enklaste och vanligaste sättet är att använda operatorn "+".

```Java
// Skapar två strängar
String str1 = "Hej ";
String str2 = "världen!";

// Sammanfogar strängarna med "+" operatorn
String resultat = str1 + str2;

// Skriver ut resultatet
System.out.println(resultat);

// Output: Hej världen!
```

Det går också att använda metoden `concat()` för att sammanfoga strängar.

```Java
// Skapar två strängar
String str1 = "Hej ";
String str2 = "världen!";

// Sammanfogar strängarna med `concat()` metoden
String resultat = str1.concat(str2);

// Skriver ut resultatet
System.out.println(resultat);

// Output: Hej världen!
```

Det är också möjligt att sammanfoga flera strängar på en gång med hjälp av `StringBuilder` klassen.

```Java
// Skapar en `StringBuilder` objekt
StringBuilder sb = new StringBuilder();

// Lägger till strängar till objektet
sb.append("Tack ");
sb.append("för ");
sb.append("att ");
sb.append("ni ");
sb.append("läser ");

// Omvandlar `StringBuilder` till en sträng och skriver ut resultatet
System.out.println(sb.toString());

// Output: Tack för att ni läser
```

## Deep Dive
Närmanvänder sig av "+" operatorn eller `concat()` metoden för att sammanfoga strängar, skapar man faktiskt en ny sträng objekt varje gång. Det innebär att om du har många strängar att sammanfoga, så kommer det att finnas många onödiga objekt i minnet. För att undvika det så är det bättre att använda `StringBuilder` klassen när man sammanfogar flera strängar. `StringBuilder` är mer effektiv och lägger inte till onödiga objekt i minnet.

Det går också att använda `String.format()` för att sammanfoga strängar på ett mer dynamiskt sätt. Här kan du infoga variabler och text på ett enkelt sätt.

```Java
// Skapar en variabel för namn
String namn = "Anna";

// Använder `String.format()` för att skapa en ny sträng
String hälsning = String.format("Hej %s, välkommen!", namn);

// Skriver ut resultatet
System.out.println(hälsning);

// Output: Hej Anna, välkommen!
```

## Se även
För mer information och exempel på hur man kan sammanfoga strängar i Java, rekommenderar vi att ni tittar på följande länkar:

- [Java String concatenation](https://www.javatpoint.com/string-concatenation-in-java)
- [String Concatenation in Java](https://www.geeksforgeeks.org/string-concatenation-java/)
- [StringBuilder Class in Java](https://www.tutorialspoint.com/java/lang/stringbuilder_append_string.htm)

Tack för att ni läste och lycka till med att sammanfoga strängar i Java!