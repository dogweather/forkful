---
title:                "C: Radera karaktärer som matchar ett mönster"
simple_title:         "Radera karaktärer som matchar ett mönster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Det finns många situationer där en programmerare kan behöva ta bort karaktärer som matchar ett visst mönster. Det kan vara för att rensa en sträng från oönskade tecken eller för att utföra en specifik manipulation av data. Oavsett syftet är kunskap om att ta bort karaktärer som matchar ett mönster en viktig färdighet för alla C-programmerare.

## Hur man gör det

Det finns många sätt att ta bort karaktärer som matchar ett mönster i C, men här är två vanliga metoder:

### Metod 1: Använda en While-loop och String.h-biblioteket
En enkel och effektiv metod är att använda en while-loop och funktionen "strchr" från String.h-biblioteket. Här är ett exempel på kod som tar bort alla förekomster av en viss karaktär i en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej alla C-programmerare!";
    char to_be_removed = 'a';

    // Loopar igenom alla karaktärer i strängen
    int i = 0;
    while (str[i]) {
        // Om den nuvarande karaktären matchar den som ska tas bort 
        if (str[i] == to_be_removed) {
            // Ersätter den med ett tomt tecken
            memmove(&str[i], &str[i + 1], strlen(str) - i);
        } else {
            // Annars fortsätter loopen
            i++;
        }
    }

    printf("%s", str);
    return 0;
}

```

**Output:**
> Hej ll C-progrmmerre!

### Metod 2: Använda funktionen "strtok" och String.h-biblioteket
En annan metod är att använda funktionen "strtok" från String.h-biblioteket för att dela upp strängen vid ett visst tecken och sedan sätta ihop den utan de karaktärer som matchar mönstret. Här är ett exempel på kod som tar bort alla mellanslag i en sträng:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej alla C-programmerare!";

    // Delar upp strängen vid mellanslag 
    char *token = strtok(str, " ");

    // Loopar tills alla delar av strängen har behandlats
    while (token) {
        // Skriver ut den aktuella delen av strängen 
        printf("%s", token);
        // Hämtar nästa del av strängen
        token = strtok(NULL, " ");
    }

    return 0;
}

```

**Output:**
> HejallaC-programmerare!

## Djupdykning
Båda metoderna ovan kan anpassas för att ta bort flera karaktärer eller ett mer komplext mönster. Det finns också andra funktioner i String.h-biblioteket som kan vara användbara beroende på vilken situation du står inför. Det är också viktigt att notera att det alltid är viktigt att hantera minnesåtkomst på ett säkert sätt när du manipulerar strängar, särskilt om du tar bort karaktärer från en redan allokerad sträng.

## Se även
- [String.h-biblioteket i C](https://www.cplusplus.com/reference/cstring/)
- [C-tutorial: Strängmanipulation](https://www.programiz.com/c-programming/c-strings)