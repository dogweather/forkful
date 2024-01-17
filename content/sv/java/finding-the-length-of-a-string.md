---
title:                "Att hitta längden på en sträng"
html_title:           "Java: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Vad & Varför?
Att hitta längden på en sträng innebär helt enkelt att räkna antalet tecken i en given sträng. Detta är en vanlig uppgift för programmerare eftersom det hjälper till att hantera och manipulera text på ett effektivt sätt.

Hur du gör:
Här är en kodexempel i Java för att hitta längden på en sträng:

```Java
String str = "Hejsan!";
int length = str.length();
System.out.println("Strängen är " + length + " tecken lång.");
```

Output:

```Java
Strängen är 7 tecken lång.
```

En annan metod som också kan användas är `str.toCharArray().length`, som konverterar strängen till en teckenlista och räknar sedan antalet tecken.

För att hitta längden på en sträng med användarinput, kan `Scanner`-klassen användas för att läsa in en sträng från användaren och sedan tillämpa `length()`-metoden för att räkna antalet tecken.

Djupdykning:
Att hitta längden på en sträng är en viktig del av textbehandling i programmering. Det är en vanlig uppgift som behövs för att beräkna eller manipulera strängar, som att kontrollera om en sträng är tom eller hitta en viss del av en sträng. Det är också en grundläggande kunskap som behövs för att förstå mer avancerade koncept inom textbehandling.

I Java finns det flera alternativ för att hitta längden på en sträng. Utöver `length()`-metoden, som användes i exempelkoden ovan, kan `str.toCharArray().length` användas, liksom `str.getBytes().length`. Båda dessa metoder räknar också antalet tecken i en sträng.

Implementationen av `length()`-metoden inkluderar en variabel vid namn `count` som håller koll på antalet loopar som behövs för att hitta längden på strängen. Sedan returneras denna `count`-variabel, som är antalet tecken i strängen. 

Se även:
- Officiell Java-dokumentation för `String.length()`-metoden: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length-- 
- Javavideodemonstration om hur man hittar längden på en sträng: https://youtu.be/06iTMcDl5cA