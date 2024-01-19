---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?

Strängkonkatenering i Java innebär att sammankoppla två eller flera strängar till en enda sträng. Programmerare gör detta för att slå ihop data i olika format eller att skapa dynamiska strängar.

## Hur man gör:
Konkatenering av strängar kan göras på flera sätt:

1. Med '+'- operatorn:
```Java
String str1 = "Hej";
String str2 = "Sverige";
String str3 = str1 + ", " + str2;
System.out.println(str3);
```
Output: `Hej, Sverige`

2. Med `concat()`- metoden:
```Java
String str1 = "Hej";
String str2 = "Sverige";
String str3 = str1.concat(", ").concat(str2);
System.out.println(str3);
```
Output: `Hej, Sverige`

## Djupdykning
Historiskt sett har '+'-operatorn länge varit det standardiserade sättet att konkatenera strängar i Java. Detta leder dock till prestandaproblem eftersom varje konkatenering skapar ett nytt strängobjekt.

Alternativ till konkatenering inkluderar `StringBuilder` och `StringBuffer` klasserna, vilka är mer effektiva för stora mängder data.

```Java
StringBuilder sb = new StringBuilder("Hej");
sb.append(", ").append("Sverige");
System.out.println(sb.toString());
```
Output: `Hej, Sverige`

Vid implementation av strängkonkaterning, måste man vara medveten om prestanda och minnesåtgång, särskilt i stora system.

## Se även:
- [Oracle Java docs för String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Oracle Java docs för StringBuilder class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Oracle Java docs för StringBuffer class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)