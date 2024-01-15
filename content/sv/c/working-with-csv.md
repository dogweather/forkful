---
title:                "Arbeta med csv"
html_title:           "C: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med CSV-filer är en vanlig uppgift när man hanterar data i olika program och verktyg. Det kan till exempel vara användbart för att importera eller exportera data från en databas, eller för att dela data med andra användare.

## Så här gör du

Att läsa och skriva CSV-filer med C är relativt enkelt. För att läsa en fil, kan du använda funktionen `fopen` för att öppna filen och sedan använda `fgets` för att läsa in varje rad. För att skriva till en fil kan du använda `fprintf` för att skriva data i det önskade formatet.

````C
#include <stdio.h>

int main() {
  // Öppna en fil för läsning
  FILE *in_file = fopen("data.csv", "r");
  
  // Loopa igenom filen och läsa in varje rad
  char line[100]; // Antag att varje rad är högst 100 tecken lång
  while(fgets(line, 100, in_file) != NULL) {
    // Hantera raden här
    printf("%s", line);
  }

  // Stäng filen
  fclose(in_file);

  // Öppna en fil för skrivning
  FILE *out_file = fopen("output.csv", "w");
  
  // Skriva data i önskat format
  fprintf(out_file, "Kolumn 1, Kolumn 2, Kolumn 3\n");
  fprintf(out_file, "Data 1, Data 2, Data 3\n");
  fprintf(out_file, "Data 4, Data 5, Data 6\n");

  // Stäng filen
  fclose(out_file);

  return 0;
}
````

## Djupdykning

CSV-filer är ett vanligt format för att lagra tabellformad data. Det finns dock vissa utmaningar med att arbeta med CSV-filer, exempelvis att hantera tecken som kommatering och citattecken. För att hantera dessa problem kan du använda `fgetc` för att hantera tecken för tecken istället för rad för rad. Dessutom kan du använda `strtok` för att dela upp raderna i enskilda celler.

## Se även

- [fopen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [fgets](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)
- [fprintf](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [fgetc](https://www.tutorialspoint.com/c_standard_library/c_function_fgetc.htm)
- [strtok](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)