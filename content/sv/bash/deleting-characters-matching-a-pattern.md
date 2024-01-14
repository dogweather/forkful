---
title:    "Bash: Radera tecken som matchar ett mönster"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster kan vara användbart för att rensa upp data eller för att förbereda det för vidare bearbetning. Genom att använda Bash programming kan du enkelt skriva kod för att filtrera och ta bort oönskade tecken på ett enkelt sätt.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i Bash, kan du använda kommandot "sed". Detta kommando används för att manipulera textsträngar och kan användas för att ta bort tecken baserat på ett visst uttryck. Det finns olika sätt att använda "sed" för att ta bort tecken, men här är ett exempel som tar bort alla siffror från en textsträng:

```Bash
sed 's/[0-9]//g' filnamn.txt
```

I det här exemplet används "s" för att ersätta och "[0-9]" för att matcha alla siffror. Den andra delen av uttrycket "g" betyder globalt, vilket innebär att alla matchande förekomster av siffror kommer att tas bort från textsträngen. Till sist anges namnet på filen som behandlas.

När detta kommando körs, kommer alla siffror som finns i filen "filnamn.txt" att tas bort och resultatet kommer att skrivas ut i terminalen. Du kan också använda "sed" tillsammans med andra kommandon som "grep" eller "awk" för att ytterligare filtrera och manipulera din textsträng.

## Djupdykning

"sed" är ett kraftfullt verktyg med många möjligheter för att matcha och manipulera text. Du kan använda olika mönster för att matcha tecken som du vill ta bort och det finns också möjlighet att använda regelbundna uttryck för att utöka funktionaliteten.

Det finns också andra sätt att ta bort tecken från en textsträng i Bash, som att använda "tr" eller "cut" kommandon. Det är viktigt att välja rätt kommando och metod som passar ditt specifika tillämpningsfall.

## Se även

Här nedan finns några användbara länkar för dig som vill lära dig mer om att ta bort tecken med hjälp av Bash programming:

- https://www.gnu.org/software/sed/manual/html_node/sed-regular-expressions.html
- https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/
- https://www.tecmint.com/delete-certain-lines-of-a-file-in-linux-using-sed-command/
- https://www.shellscript.sh/tips/dontdelete.html