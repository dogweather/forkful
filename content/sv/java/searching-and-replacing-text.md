---
title:                "Java: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Att söka och ersätta text är en viktig del av programmering. Det hjälper dig att ändra stora mängder text på en gång, vilket gör koden mer effektiv och sparar tid.

# Hur man gör

Att söka och ersätta text i Java är enkelt med hjälp av metoden `replaceAll()`. Här är ett exempel på hur man använder denna metod för att byta ut alla förekomster av "hej" med "hej hej":

```Java
String text = "Hej, jag heter Alice!";
String newText = text.replaceAll("hej", "hej hej");
System.out.println(newText);
```

Output:
```
Hej hej, jag heter Alice!
```

Här ser vi att alla förekomster av "hej" i strängen har ersatts med "hej hej". Metoden `replaceAll()` tar emot två argument - den första är den text du vill ersätta och den andra är den nya texten du vill använda.

# Djupdykning

När du använder `replaceAll()`-metoden, behöver du vara medveten om att det är en fallkänslig process. Det betyder att om du har en sträng som innehåller både stora och små bokstäver och du använder metoden för att byta ut en bokstav, kommer den bara att ersätta bokstäver som matchar exakt. Om du vill byta ut bokstäver oavsett deras storlek, kan du använda metoden `replace()` istället.

En annan viktig sak att komma ihåg är att metoden `replaceAll()` använder en särskild form av regex (regular expressions) för att söka och ersätta text. Det innebär att du kan använda olika specialtecken för att göra mer avancerade sökningar och ersättningar. Till exempel kan du använda `.+` för att ersätta alla bokstäver och symboler mellan två visst angivna tecken.

# Se även

- [Java String API Reference](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Regular Expressions in Java](https://www.codejava.net/java-se/text/regular-expressions-regex-api-tutorial-for-java)