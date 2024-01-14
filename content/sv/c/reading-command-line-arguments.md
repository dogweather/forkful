---
title:    "C: Läsning av kommandoradsargument"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument kan verka som en enkel uppgift, men det kan faktiskt vara mycket användbart för utvecklare. Genom att kunna läsa kommandoradsargument kan du skriva program som är mer dynamiska och interaktiva för användaren. Det gör programmet mer användarvänligt och kan spara tid och ansträngning för både utvecklare och användare.

## Hur man

För att läsa kommandoradsargument i ett C-program behöver du först inkludera "stdio.h" biblioteket i ditt program. Därefter måste du definiera en funktion "main" som tar emot två parametrar: "argc" och "argv". "argc" är antalet argument som läses in och "argv" är en array av strängar som representerar de faktiska argumenten. Nedan följer ett enkelt exempel på hur du kan läsa kommandoradsargument:

\`\`\`C
#include <stdio.h>

int main(int argc, char *argv[]) {
	printf("Antal argument: %d\n", argc);
	for (int i = 0; i < argc; i++) {
		printf("Argument %d: %s\n", i + 1, argv[i]);
	}
	return 0;
}
\`\`\`

När du kör programmet och lägger till argument efter programnamnet, till exempel "programnamn argument1 argument2", kommer du att se följande utmatning:

Antal argument: 3
Argument 1: programnamn
Argument 2: argument1
Argument 3: argument2

Som du kan se kommer antalet argument och själva argumenten att skrivas ut. Detta är ett enkelt exempel, men du kan göra mycket mer med kommandoradsargument. Du kan läsa in specifika argument och använda dem för att utföra olika uppgifter i ditt program.

## Djupdykning

Ett annat sätt att läsa kommandoradsargument är att använda funktionen "getopt". Denna funktion kan användas för att läsa in specifika flaggor och värden från kommandoraden. Detta är särskilt användbart om ditt program har många olika alternativ och du vill ha en enklare och mer strukturerad lösning för att läsa in dem. Du kan läsa mer om "getopt" och hur du använder den i C i [denna artikel](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html).

## Se även

- [C Command Line Arguments Tutorial](https://www.programiz.com/c-programming/c-command-line-arguments)
- [Pass Command Line Arguments to a C Program - GeeksforGeeks](https://www.geeksforgeeks.org/pass-command-line-arguments-to-a-c-program/)
- [Argument Parsing in C: The getopt() Function](https://www.opennet.ru/docs/RUS/glibc/glibc-17.html) (på ryska)