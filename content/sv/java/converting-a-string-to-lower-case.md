---
title:                "Konvertera en sträng till gemener"
html_title:           "Java: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till små bokstäver i Java är en vanlig uppgift för programmerare. Det innebär att man omvandlar alla bokstäver i en sträng till små bokstäver, oavsett om strängen består av både små och stora bokstäver. Detta behövs ofta för att underlätta jämförelser och sökningar i textsträngar.

## Så här:
För att konvertera en sträng till små bokstäver behöver vi använda metoden `toLowerCase()` i Java. Här är ett enkelt exempel på hur man kan göra det:
```Java
String str = "Hej";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr); // output: hej
```

Det är också möjligt att konvertera strängen "på plats" genom att använda metoden `toLowerCase()` på varje enskild bokstav i strängen. Detta kan se ut så här:
```Java
String str = "Hej";
char[] chars = str.toCharArray();
for(int i = 0; i < chars.length; i++){
    chars[i] = Character.toLowerCase(chars[i]);
}
String lowerCaseStr = new String(chars);
System.out.println(lowerCaseStr); // output: hej
```

## Utforska djupare:
Konvertering av strängar till små bokstäver är ett vanligt problem som programmerare har stött på under lång tid. Faktum är att detta var en vanlig uppgift i äldre versioner av Java, eftersom strängar då behandlades som en array av tecken istället för en egen klass.

Ett alternativ till att använda `toLowerCase()` är att använda en `StringBuilder`-klass och dess `append()`-metod för att lägga till de små bokstäverna till en tom sträng. Detta kan vara mer effektivt för stora strängar, men det är viktigt att komma ihåg att det ändå innebär en extra loop.

## Se även:
- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [StringBuilder Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Java Character Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)