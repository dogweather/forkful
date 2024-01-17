---
title:                "Sammanslagning av strängar"
html_title:           "C: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

Vad är strängkonkatinering och varför gör programmerare det?

Strängkonkatenering är när man sammanslår flera strängar till en enda sträng. Detta är användbart när man vill skapa en längre sträng utifrån flera mindre delar. Programmerare använder detta för att bygga dynamiska strängar, till exempel i ett meddelande eller en filväg.

Så här gör du det:

```C
// Skapa två strängar
char förnamn[] = "Karin";
char efternamn[] = "Svensson";

// Konkatenera dem till en ny sträng
char namn[50];
strcat(namn, förnamn);
strcat(namn, " ");
strcat(namn, efternamn);

// Skriv ut den nya strängen
printf("Namn: %s", namn);

// Output: Namn: Karin Svensson
```

Djupdykning:

Historiskt sett har strängkonkatenering varit en viktig funktion i programmeringsspråk. I äldre språk, som Fortran och COBOL, var det vanligt att använda förutbestämda storlekar på strängarna, vilket gjorde det viktigt att kunna sammanslå strängar för att undvika att överskrida gränserna.

I moderna programmeringsspråk finns det dock alternativ till strängkonkatenering, som till exempel möjligheten att skapa dynamiska strängar. I vissa språk, som Python, är strängar även immutabla vilket innebär att de inte kan ändras vilket gör strängkonkatenering mindre användbart.

Implementeringen av strängkonkatenering kan variera beroende på programmeringsspråk och dess funktioner. I C använder man funktionen `strcat()` för att sammanslå strängar, medan man i andra språk kanske använder en operator som `+`.

Se även:

- Dokumentation för `strcat()`: https://www.programiz.com/c-programming/library-function/string.h/strcat

- En jämförelse mellan C och andra programmeringsspråk: https://hackr.io/blog/c-programming-vs-python