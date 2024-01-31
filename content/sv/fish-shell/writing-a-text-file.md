---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att lagra textdata i en fil på disken. Programmerare använder detta för att spara konfigurationer, loggar eller för att kommunicera med andra program.

## Hur gör man:
```Fish Shell
echo "Hej, detta är en rad text" > minfil.txt  # Skapar en ny fil eller skriver över den gamla
cat minfil.txt  # Visar innehållet i filen
```
```Fish Shell
echo "Ytterligare en rad text" >> minfil.txt  # Lägger till text i befintlig fil
cat minfil.txt
```
Output:
```
Hej, detta är en rad text
Ytterligare en rad text
```

## Fördjupning
Historiskt sett har terminalen och kommandotolkar varit viktiga för att hantera filer på Unix-system. Fish Shell, en modern och användarvänlig kommandotolk, erbjuder funktioner som `echo` för att skriva textfiler effektivt. Alternativ inkluderar skriptningsspråk som Python eller Bash. Detaljer som att använda flaggor för att styra skrivläge ('>' för att skapa/ersätta, '>>' för att lägga till) är viktiga att förstå för att undvika dataförlust.

## Se även
- Fish Shell dokumentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Bash vs Fish: [https://www.slant.co/versus/126/127/~fish_vs_bash](https://www.slant.co/versus/126/127/~fish_vs_bash)
