---
title:                "C: Skriva till standard error"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Skriva till standardfel kan verka som ett enkelt koncept, men varför skulle någon vilja göra det? Det finns faktiskt flera användbara anledningar till att kunna skriva till standardfel i dina C-program.

## Hur man gör det

Att skriva till standardfel i C är faktiskt ganska enkelt. Du behöver bara använda funktionen "fprintf" och ange "stderr" som den första parameteren. Här är ett exempel:

```C
fprintf(stderr, "Det här är ett felmeddelande\n");
```

Detta kommer att skriva ut texten "Det här är ett felmeddelande" till standardfelkanalen. Du kan också använda "fputs" för att skriva en befintlig sträng till standardfel:

```C
char* error = "Ett annat felmeddelande";
fputs(error, stderr);
```

Du kan också kombinera användningen av "fprintf" och "fputs" för att skriva ut mer detaljerade felmeddelanden. Här är ett exempel på hur du kan använda båda funktionerna tillsammans:

```C
fprintf(stderr, "Ett allvarligt fel inträffade: ");
fputs(error, stderr);
```

Detta kommer att skriva ut "Ett allvarligt fel inträffade: Ett annat felmeddelande" till standardfelkanalen.

En annan användbar funktion för att skriva till standardfel är "perror", som automatiskt skriver ut en felbeskrivning baserad på det senaste felmeddelandet som lagras i "errno" -variabeln. Här är ett exempel på hur du kan använda "perror":

```C
fp = fopen("saknad_fil.txt", "r");
if (fp == NULL) {
	perror("Det gick inte att öppna filen");
	exit(1);
}
```

Om filen "saknad_fil.txt" inte kan öppnas, kommer "perror" att skriva ut "Det gick inte att öppna filen: Filen eller katalogen finns inte" till standardfelkanalen.

## Djupare dykning

Genom att skriva till standardfel kan du enkelt skapa mer informativa och användbara felmeddelanden i dina C-program. Genom att kombinera "fprintf", "fputs" och "perror" kan du skapa anpassade felmeddelanden som hjälper dig att spåra och lösa problem i ditt program.

Det är också viktigt att notera att "stderr" i princip är en filhante som representerar standardfelkanalen. Det betyder att du kan använda alla funktioner som du skulle använda för att skriva till en vanlig fil, som t.ex. "fprintf", "fputs" och "fwrite", för att skriva till standardfel.

## Se även

- [C programmets officiella dokumentation på stderr](https://www.programiz.com/c-programming/library-function/stdio.h/stderr)
- [En översikt över hur du hanterar fel i C-program](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
- [Ett praktiskt exempel på hur du kan använda "stderr" i C](https://www.geeksforgeeks.org/standard-error-streams-in-c/)