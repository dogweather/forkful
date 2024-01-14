---
title:                "C++: Skapa en tillfällig fil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför skapa en temporär fil?

Att skapa en temporär fil är en vanlig teknik inom C++ programmering för att hantera data som behöver tillfälligt lagras. Det kan vara användbart i situationer där man vill undvika att permanent ändra eller förstöra befintliga filer, till exempel när man bearbetar stora mängder data eller när man behöver skapa en temporär fil som endast är relevant för en specifik körning av programmet.

## Hur man skapar en temporär fil

Att skapa en temporär fil i C++ är enkelt och görs genom att använda standardbiblioteksfunktionen `tmpnam()`. Den returnerar en unik filnamn som kan användas för att skriva och läsa data från filen. Nedan finns ett exempel på hur man kan använda denna funktion:

```C++
#include <cstdio>

int main() {
  char* filename = tmpnam(nullptr); // Skapa ett unikt filnamn
  FILE* fp = fopen(filename, "w"); // Öppna fil i skrivläge
  fprintf(fp, "Hej från min temporära fil!"); // Skriv data till filen
  fclose(fp); // Stäng filen när man är klar
  remove(filename); // Ta bort den temporära filen
  return 0;
}
```

När koden ovan körs, kommer filen att skapas och texten "Hej från min temporära fil!" kommer att skrivas till filen. Sedan stängs filen och tas bort för att inte längre vara tillgänglig på filsystemet.

## Djupdykning i skapandet av temporära filer

I bakgrunden använder funktionen `tmpnam()` olika metoder för att skapa ett unikt filnamn. Det kan till exempel baseras på process-ID, tidsstämpel eller andra systemparametrar. Dessa filnamn kommer alltid att vara unika, även om flera processer körs samtidigt.

När en temporär fil skapas, lagras den vanligtvis i den temporära mappen på operativsystemet. Detta är en speciell mapp som används för att lagra temporära filer som inte behövs för en längre tid. Det är viktigt att notera att dessa temporära filer kan tas bort av operativsystemet när de inte längre behövs, så det är viktigt att stänga och ta bort filen när man är klar med den.

# Se även

- [C++ Standardbibliotek](https://en.cppreference.com/w/cpp/filesystem/temporary_directory_path)
- [PHP - Skapa temporär fil](https://www.php.net/manual/en/function.tempnam.php)
- [Java - Skapa temporär fil](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)