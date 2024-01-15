---
title:                "Sökning och ersättning av text"
html_title:           "Bash: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Har du någonsin behövt byta ut ett ord eller en fras som upprepas flera gånger i en text? Bash erbjuder en enkel och effektiv metod för att söka och ersätta text. Detta kan vara särskilt användbart när du arbetar med stora mängder text eller behöver genomföra samma ändringar på flera filer.

## Hur man gör
För att söka och ersätta text i Bash använder man sig av kommandot "sed" (stream editor). Det finns olika sätt att använda detta kommando, men ett av de vanligaste är att skriva en enkel kommandorad med syntaxen:

```
sed 's/gammalt_ord/nytt_ord/g' filnamn.txt
```

Där "s" står för sökning (substitution), "gammalt_ord" är den text du vill ersätta, "nytt_ord" är den text du vill ersätta med och "filnamn.txt" är den fil du vill söka i. Kommandot kommer att gå igenom filen och byta ut alla förekomster av "gammalt_ord" med "nytt_ord". Om du vill byta ut flera ord kan du använda samma syntax men lägga till flera sök- och ersättningsfraser, till exempel:

```
sed 's/hund/katt/g; s/apa/igelkott/g' filnamn.txt
```

Detta kommer att byta ut alla förekomster av "hund" med "katt" och "apa" med "igelkott".

## Djupdykning
Sedan erbjuder en rad olika alternativ för sökning och ersättning, till exempel att göra sökningen icke-känslig för skillnad mellan stora och små bokstäver, begränsa sökningen till en viss rad eller en viss del av en rad, och mer. Du kan utforska dessa alternativ genom att skriva "man sed" i terminalen eller söka efter dokumentation online.

Det är också möjligt att kombinera "sed" med andra Bash-kommandon för att göra mer avancerade sökningar och ändringar i text. Till exempel kan du använda "grep" för att söka efter en viss fras i flera filer, och därefter använda "sed" för att byta ut den frasen i alla filer samtidigt.

## Se även
- [Bash Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Sed Tutorial](https://www.gnu.org/software/sed/manual/sed.html)
- [Kommandoradsväljaren (GNU)](https://www.gnu.org/software/sed/manual/sed.html)