---
title:                "Java: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en vanlig uppgift inom programmering, särskilt när man hanterar användarinput eller andra externa data. Genom att konvertera strängen till små bokstäver säkerställer man att den är enhetlig och lättare att jämföra och bearbeta.

##Hur man gör
Det finns flera olika sätt att konvertera en sträng till små bokstäver i Java. Ett av de enklaste sätten är att använda inbyggda metoder i Java String-klassen.

```Java
String str = "Hej, SWEDEN!";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```

Output:
```
hej, sweden!
```

En annan metod är att använda en for-loop för att gå igenom varje bokstav i strängen och ändra den till motsvarande små bokstav.

```Java
String str = "Välkommen till JAVABLOGG!";
String lowerCaseStr = "";
for (int i = 0; i < str.length(); i++) {
  lowerCaseStr += Character.toLowerCase(str.charAt(i));
}
System.out.println(lowerCaseStr);
```

Output:
```
välkommen till javablogg!
```

En tredje metod är att använda StringBuilder-klassen för att bygga en ny sträng med små bokstäver.

```Java
String str = "Hej, DETTA ÄR EN JAVABLOGG!";
StringBuilder sb = new StringBuilder();
for (char c : str.toCharArray()) {
  if (Character.isUpperCase(c)) {
    sb.append(Character.toLowerCase(c));
  } else {
    sb.append(c);
  }
}
String lowerCaseStr = sb.toString();
System.out.println(lowerCaseStr);
```

Output:
```
hej, detta är en javablogg!
```

## Djupdykning
Det är viktigt att notera att alla dessa metoder returnerar en ny sträng, istället för att modifiera den ursprungliga strängen. Detta beror på att strängar i Java är immutabla, vilket innebär att de inte kan ändras. Därför är det viktigt att tilldela resultatet av konverteringsmetoderna till en ny variabel, som vi har gjort i exemplen ovan.

Det finns också några andra saker att tänka på när man konverterar strängar till små bokstäver. För det första, om strängen redan är i små bokstäver kommer ingen konvertering att ske. Det kan också finnas språk- eller teckenberoenden skillnader när man konverterar strängar till små bokstäver. Därför är det alltid viktigt att testa koden på olika typer av data för att säkerställa att den fungerar som förväntat.

## Se även
- [Java String-klassen JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorialspoint Java String-översikt](https://www.tutorialspoint.com/java/java_string_unnecessary_equal.htm)