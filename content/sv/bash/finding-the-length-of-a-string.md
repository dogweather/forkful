---
title:                "Att hitta längden på en sträng"
html_title:           "Bash: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Det finns många olika användningsområden för att hitta längden på en sträng inom Bash programmering. Det kan vara användbart för att kontrollera inmatade användardata, manipulera textsträngar eller skapa dynamiska variabler.

## Så här gör du
För att hitta längden på en sträng i Bash, kan du använda inbyggda kommandon som "echo" och "wc". Se nedan för ett kodexempel och tillhörande utmatning.

```Bash
# Skapa en variabel med en sträng
string="Hej, välkommen till min artikel!"

# Använd "echo" och skicka variabeln till "wc" för att räkna antalet tecken
echo $string | wc -c

# Detta kommer att ge utmatningen "31", vilket är längden på strängen inklusive mellanslag.
```

En annan metod är att använda "expr" kommandot tillsammans med "length" funktionen. Se exempel nedan.

```Bash
# Skapa en variabel med en sträng
string="Hej, välkommen till min artikel!"

# Använd "expr" och "length" för att räkna antalet tecken i strängen
expr length $string

# Detta kommer att ge utmatningen "31", vilket är längden på strängen inklusive mellanslag.
```

## Djupdykning
Att hitta längden på en sträng i Bash kan vara användbart när du arbetar med datahantering, särskilt när du behöver hantera användarinput. Det är också ett enkelt sätt att manipulera textsträngar eller skapa dynamiska variabler genom att använda längden på en sträng som en variabel.

Du kan också använda olika alternativ med "wc" eller "length" baserat på ditt behov. Till exempel kan du lägga till flaggor som "-l" för att räkna antalet rader eller "-w" för att räkna antalet ord i en sträng.

## Se även
- [Bash Guide för Nybörjare](https://tiswww.case.edu/php/chet/bash/bashref.html)
- [wc man-sida](https://www.man7.org/linux/man-pages/man1/wc.1.html)
- [expr man-sida](https://www.man7.org/linux/man-pages/man1/expr.1.html)