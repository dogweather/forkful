---
title:                "Läsning av kommandoradsargument"
html_title:           "C: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Vad & Varför?

Läsning av kommandoradsargument är en vanlig uppgift i programmering som innebär att ta emot information från användaren genom kommandoraden. Detta gör det möjligt för användare att anpassa programmets beteende och öka dess användbarhet.

Hur man:
```c
int main(int argc, char *argv[]) { 
    printf("Antalet kommandoradsargument är: %d\n", argc); 
    printf("De givna argumenten är:\n"); 
    for (int i = 0; i < argc; i++) {
        printf("%s\n", argv[i]); 
    } 
    return 0; 
}
```

Om programmet körs från kommandoraden med argument som "hello" och "world", kommer utmatningen att vara:
```
Antalet kommandoradsargument är: 3
De givna argumenten är:
programnamnet
hello
world
```

Djupt dyk:
Läsning av kommandoradsargument har funnits sedan de tidigaste dagarna av programmering och har en betydande roll i att göra programmen mer anpassningsbara och användarvänliga. Det finns också alternativ till att läsa kommandoradsargument, till exempel att använda en konfigurationsfil, men kommandoradsargument är enklare för mindre program.

Se även:
- [https://www.programiz.com/c-programming/c-command-line-arguments](https://www.programiz.com/c-programming/c-command-line-arguments)
- [https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [https://www.techonthenet.com/c_language/standard_library_functions/stdio_h/main.php](https://www.techonthenet.com/c_language/standard_library_functions/stdio_h/main.php)