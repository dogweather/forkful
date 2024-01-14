---
title:                "C: Sammanslående strängar"
simple_title:         "Sammanslående strängar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
En av de grundläggande koncepten inom C-programmering är möjligheten att manipulera strängar. Att sammanslå flera strängar till en enda är en viktig teknik som används i många program. Det kan vara till nytta för att skapa textutskrifter, lösa arkitekturmönster eller helt enkelt för att organisera data på ett mer effektivt sätt. 

## Så här gör du
Att sammanslå strängar i C är relativt enkelt. Det finns flera sätt att göra det på, men den enklaste metoden är att använda "strcat()" funktionen. För detta behöver du två strängar som redan är deklarerade och initialiserade. Sedan använder du funktionen för att sammanslå dem och lagra resultatet i en tredje sträng. 

```C
char str1[] = "Hej";
char str2[] = "världen!";
char str3[100];

// Sammanslå str1 och str2
strcat(str3, str1);
strcat(str3, str2);

printf("%s", str3); // Utskrift: Hejvärlden!
```

I ovanstående exempel sammanslår vi strängarna "Hej" och "världen!" och lagrar resultatet i str3. För att kunna använda funktionen "strcat()" behöver vi inkludera "string.h" biblioteket.

## Djupdykning
Om vi tittar närmare på "strcat()" funktionen kan vi se att den tar två argument - destinationsträngen och källsträngen. Destinationsträngen är där det sammanslagna resultatet kommer att lagras och källsträngen är den sträng som ska sättas ihop med destinationsträngen. Det är också viktigt att notera att destinationsträngen måste vara tillräckligt stor för att rymma båda strängarna.

Det finns också andra funktioner som kan användas för att sammanslå strängar i C, såsom "strncat()" som låter dig sätta ett maximalt antal tecken från källsträngen och "sprintf()" som låter dig sätta ihop flera variabler och strängar till en enda sträng.

## Se även
- Läs mer om "strcat()" funktionen här: [https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- Här kan du lära dig mer om hur du använder "sprintf()" funktionen: [https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)
- Om du vill utforska fler sätt att manipulera strängar i C: [https://www.geeksforgeeks.org/c-string-manipulation-basics/](https://www.geeksforgeeks.org/c-string-manipulation-basics/)

Se även: 
[https://www.programiz.com/c-programming/string-concatenation](https://www.programiz.com/c-programming/string-concatenation) 
[https://www.ee.iitb.ac.in/~nilotpal/String_Concatenation.html](https://www.ee.iitb.ac.in/~nilotpal/String_Concatenation.html)