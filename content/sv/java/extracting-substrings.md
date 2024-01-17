---
title:                "Utvinna delsträngar"
html_title:           "Java: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är ett sätt för programmerare att få specifika delar av en textsträng som uppfyller vissa kriterier. Det används ofta för att hantera data från användarinmatning eller för att manipulera text på ett mer precist sätt.

## Så här gör du:
Java har inbyggda metoder för att extrahera substrängar från befintliga strängar. Här är ett exempel på hur man kan extrahera en delsträng från ett användarinmatat lösenord:

```Java
String password = "supersekretochlångtlösenord";
String extracted = password.substring(0, 6);
System.out.println(extracted);
```

Detta kodexempel kommer att skriva ut de första sex tecknen från lösenordet, vilket i detta fall skulle vara "superse". Observera att indexeringen börjar från 0, så 0 är det första tecknet och 6 är det sjunde tecknet.

## Djupdykning:
Att extrahera substrängar är en vanlig operation inom programmering och det finns flera olika sätt att göra det på. I Java kan man också använda metoden `substring()` för att extrahera en delsträng baserat på ett specifikt tecken eller ett teckens intervall. Det finns också andra typer av strängmanipulation som kan användas för att uppnå samma resultat, som regex-uttryck eller split-funktionen.

## Se även:
- <https://www.w3schools.com/java/java_strings_substring.asp>
- <https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#substring(int,%20int)>