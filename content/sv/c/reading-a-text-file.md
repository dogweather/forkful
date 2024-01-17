---
title:                "Läsa en textfil"
html_title:           "C: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Läsning av en textfil i ett C-program är processen att öppna, läsa och bearbeta innehållet av en textfil. Programmers gör detta för att få åtkomst till data från en extern källa och använda den i deras program.

## Hur man:
För att läsa en textfil i C, börja med att öppna filen med hjälp av fopen() funktionen. Sedan använder du fgets() för att läsa innehållet av filen rad för rad. Du kan också använda fscanf() för att läsa in specifika delar av filen.

```C
int main() {
  FILE *fp;
  char line[100];

  // öppna filen för läsning
  fp = fopen("text.txt", "r");

  // läs filen rad för rad och skriv ut
  while (fgets(line, 100, fp) != NULL) {
    printf("%s", line);
  }

  // stäng filen
  fclose(fp);

  return 0;
}
```

### Konsolresultat:
```
Detta är rad ett. Detta är rad två.
Detta är rad tre. Detta är rad fyra.
```

## Djupdykning:
Läsning av textfiler är en viktig del av programering. Det används ofta för att läsa indata eller konfigurationsfiler för ett program. Det finns också alternativ för att läsa binära filer eller att använda bibliotek som tillhandahåller enklare funktioner för att läsa filer.

## Se även:
- Beskrivning av fopen() funktionen i C-programmering: https://www.programiz.com/c-programming/c-file-input-output
- Dokumentation för fgets() och fscanf() funktionerna i C-programmering: https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm