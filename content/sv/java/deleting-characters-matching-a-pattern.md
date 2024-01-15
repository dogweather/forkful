---
title:                "Att ta bort tecken som matchar ett mönster"
html_title:           "Java: Att ta bort tecken som matchar ett mönster"
simple_title:         "Att ta bort tecken som matchar ett mönster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Ibland vill man ta bort vissa tecken från en textsträng baserat på ett visst mönster. Det kan till exempel vara för att upprätthålla en specifik formatering eller för att rensa bort onödiga tecken.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i en textsträng, kan man använda sig av Java's `replaceAll()`-metod tillsammans med regular expressions. Här är ett enkelt exempel som tar bort alla siffror från en textsträng:

```Java
String originalStrang = "Jag har 3 hundar och 5 katter.";
String nyStrang = originalStrang.replaceAll("[0-9]", "");

System.out.println(nyStrang);
```

Output: "Jag har hundar och katter."

I det här exemplet används `replaceAll()`-metoden för att söka igenom `originalStrang` och ersätta alla siffror (representerade av `[0-9]`) med ett tomt tecken. Detta resulterar i den uppdaterade strängen `nyStrang`. 

Det finns olika typer av regular expressions som kan användas för att matcha olika mönster och tecken i en textsträng. Det kan vara bra att läsa på mer om dessa för att kunna använda `replaceAll()`-metoden på ett effektivt sätt.

## Djupdykning

Ibland kan man vilja ta bort vissa tecken från en sträng som inte nödvändigtvis finns i ett specifikt mönster. Då kan det vara användbart att använda sig av `replace()`-metoden istället för `replaceall()`. I motsats till `replaceAll()` som arbetar med regular expressions, så tar `replace()` emot två vanliga strängar som argument och ersätter den första strängen med den andra.

```Java
String originalStrang = "Jag gillar #pizza# och #pasta#.";
String nyStrang = originalStrang.replace("#", "");

System.out.println(nyStrang);
```

Output: "Jag gillar pizza och pasta."

Här används `replace()`-metoden för att ersätta alla förekomster av "#" med ett tomt tecken, vilket resulterar i att tecknet tas bort från strängen.

Det finns även andra metoder som kan användas för att rensa bort tecken från en sträng, såsom `substring()` och `trim()`. Det är viktigt att välja den lämpligaste metoden beroende på vilket resultat man vill uppnå.

## Se även

- Java's Regular Expressions Tutorial: https://docs.oracle.com/javase/tutorial/essential/regex/
- Java String Methods: https://www.w3schools.com/java/java_ref_string.asp