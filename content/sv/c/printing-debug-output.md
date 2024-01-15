---
title:                "Utskrift av felsökningsutdata"
html_title:           "C: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut felmeddelanden och debuggingutdata är ett viktigt verktyg för att hitta buggar och förbättra programkod. Utan att visa vad som händer i koden kan det vara svårt att förstå var problemet ligger.

## Hur man gör det

Att skriva ut felmeddelanden och debuggingutdata är enkelt i C-språket. Här är ett exempel på en funktion som skriver ut ett meddelande:

```C
void print_error(char* message) {
    printf("Error: %s\n", message);
}
```

För att använda denna funktion i koden kan man bara skicka ett meddelande som argument:

```C
print_error("The program has encountered an error.");
```

Detta kommer att skriva ut "Error: The program has encountered an error." på skärmen när koden körs.

Det är också möjligt att skriva ut värden på variabler för att få mer detaljerad utdata. Till exempel:

```C
int age = 25;
printf("My age is %d years old.\n", age);
```

Detta kommer att skriva ut "My age is 25 years old." på skärmen. Genom att använda olika formatkonverterare kan man skriva ut olika typer av variabler som t.ex. float, char eller string.

## Djupdykning

Att använda funktionen printf() är det vanligaste sättet att skriva ut debuggingutdata i C. Men det finns också andra sätt att göra det på, som t.ex. att använda funktionen fprintf() för att skriva ut till en fil istället för till skärmen.

Det finns också speciella debuggingverktyg som kan användas för att visa utdata på ett mer överskådligt sätt. Till exempel kan man använda gdb (GNU Debugger) för att stega igenom koden och se värdet på olika variabler vid olika punkter.

Att skriva ut debuggingutdata bör göras med omsorg, eftersom det kan påverka prestandan hos programmet. Se till att endast skriva ut det som är nödvändigt och ta bort utskrifter när de inte längre behövs.

## Se även

- [The Art of Debugging](https://www.amazon.com/Art-Debugging-GDB-DDD-Linux/dp/159327002X)
- [Debugging with GDB](https://www.gnu.org/software/gdb/)
- [Fprintf() function in C](https://www.geeksforgeeks.org/fprintf-in-c/#:~:text=fprintf()%20function%20in%20C%20language%20writes%20formatted%20data%20to,det.&text=Declaration%20of%20fprintf(),-int%20fprintf%28FILE%20*stream%2C%20const%20char%20*format%2C%20%2E%2E%2E%29)