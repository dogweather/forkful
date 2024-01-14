---
title:                "Java: Att stora bokstavera en sträng"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför

Att använda sig av en kapitaliserad sträng kan vara användbart i många olika sammanhang. Det kan till exempel hjälpa till att förtydliga text, göra den mer lättläslig eller för att följa ett specifikt format på en viss plattform eller enhet.

##Så här gör man

För att kapitalisera en sträng med hjälp av Java finns det flera sätt att göra det på. Ett av de enklaste sätten är att använda sig av String-klassen och dess metod toUpperCase(). Här är ett exempel:

```Java
String str = "hej på dig!";
String capitalized = str.toUpperCase();
System.out.println(capitalized);
```

Output: HEJ PÅ DIG!

För att kapitalisera endast den första bokstaven i en sträng kan man använda sig av metoden capitalize() från StringUtils-klassen i Apache Commons Text. Här är ett exempel på hur det kan se ut:

```Java
String str = "hej på dig!";
String capitalized = org.apache.commons.text.WordUtils.capitalize(str);
System.out.println(capitalized);
```

Output: Hej på dig!

En annan metod är att använda sig av String Builder-klassen och dess metod replace(). Detta kan vara praktiskt när man vill byta ut ett visst tecken eller en viss sekvens av tecken i en sträng. Här är ett exempel på hur det kan se ut:

```Java
StringBuilder sb = new StringBuilder("Hej på dig!");
sb.replace(0, 1, "H");
System.out.println(sb.toString());
```

Output: Hej på dig!

##Djupdykning

Att kapitalisera en sträng handlar om mycket mer än bara att ändra bokstävernas storlek. För att få en djupare förståelse för det kan det vara intressant att undersöka hur olika språk hanterar storleken på bokstäver.

I vissa språk, som tyska, finns det specifika regler för vilka bokstäver som ska vara kapitaliserade i början av en mening. På engelska är det vanligare med en mer avslappnad attityd till storleken på bokstäver och oftast kapitaliseras alla ord i en titel. Det finns även språk där det inte existerar någon större skillnad mellan stora och små bokstäver, som japanska.

Att förstå dessa skillnader kan vara avgörande vid programmering av en applikation som är tänkt att fungera internationellt.

##Se även

- [Java String Class](https://www.w3schools.com/java/java_string.asp)
- [Apache Commons Text StringUtils Class](https://commons.apache.org/proper/commons-text/apidocs/org/apache/commons/text/WordUtils.html)
- [Java StringBuilder Class](https://www.w3schools.com/java/java_stringbuilder.asp)