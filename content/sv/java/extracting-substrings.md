---
title:    "Java: Extrahera subträngar"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför man ska använda substrings i Java
Varför skulle någon välja att använda substrings i Java? Det finns faktiskt flera olika anledningar till varför det kan vara användbart. Kanske behöver du bara använda en del av en sträng istället för hela, eller så vill du bara extrahera en viss del för att jämföra med en annan sträng. Oavsett anledningen så är substring en viktig funktion att ha i din Java-verktygslåda.

## Hur man använder substrings i Java
Att extrahera en substräng från en befintlig sträng är enkelt i Java. Använd bara metoden `substring(int beginIndex, int endIndex)` där `beginIndex` är indexet för den första tecknet i substrängen och `endIndex` är indexet för det sista tecknet. Här är ett enkelt exempel:

```Java
String str = "Hej världen!";
String sub = str.substring(4, 9);
System.out.println(sub); // Output: värld
```

Du kan också använda enbart `indexOf()` för att få indexet för ett visst tecken eller teckensekvens och sedan använda det med metoden `substring()` för att extrahera det du behöver. Här är ett exempel på det:

```Java
String str = "Jag älskar att koda!";
int index = str.indexOf("älskar");
String sub = str.substring(index);
System.out.println(sub); // Output: älskar att koda!
```

Du kan också använda `substring()` för att jämföra delar av olika strängar. Genom att använda `equals()` kan du kontrollera om två substrängar är identiska. Här är ett exempel:

```Java
String str1 = "Hello world!";
String str2 = "Hello";
String sub = str1.substring(0, 5);
if(sub.equals(str2)) {
    System.out.println("They are the same!");
} else {
    System.out.println("Not the same.");
}
```

## Djupdykning i substrings
Nu när du vet hur man använder `substring()` och dess olika applikationer kanske du undrar hur det egentligen fungerar under huven. När du anropar metoden `substring()` så skapas en ny sträng som innehåller den önskade biten av den ursprungliga strängen.

En viktig sak att notera är att Java strängar är oföränderliga, vilket betyder att när en sträng väl är skapad kan den inte ändras. Därför skapas en ny sträng när du använder `substring()`.

## Se även
- [Java dokumentation för `String` klassen (på svenska)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java tutorial för strängar (på svenska)](http://www.mojiasoft.com/java-tutorial/strangar-i-java/)
- [Java substring på GeeksforGeeks (på engelska)](https://www.geeksforgeeks.org/java-substring-method-examples/)