---
title:    "PHP: Läsa en textfil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

##Varför läsa en textfil?

Att läsa en textfil är en viktig och grundläggande uppgift inom programmering. Många gånger innehåller våra program data som sparas i textfiler och det är därför viktigt att kunna läsa och bearbeta dessa filer för att få ut relevant information.

##Så här gör du det

För att läsa en textfil i PHP används fopen() funktionen. Först måste vi öppna filen genom att ange filnamnet och öppningsläget "r", som står för "read". Sedan kan vi loopa igenom filen och utskrift den rad för rad med hjälp av fgets() funktionen tills filen är helt läst.

```PHP
$myfile = fopen("textfil.txt", "r");
while(!feof($myfile)) {
  echo fgets($myfile) . "<br>";
}
fclose($myfile);
```

Det finns också andra sätt att läsa en textfil, såsom att använda file_get_contents() eller file() funktionerna. Det viktiga är att förstå konceptet bakom filinläsning och använda den metod som passar bäst för din kod.

##En djupdykning

När du öppnar en textfil i PHP kan du även ange öppningsläge som "w" (write) eller "a" (append) för att kunna skriva eller tillägga data till filen. Du kan också använda fseek() funktionen för att gå till ett specifikt ställe i filen och ändra data.

Det finns också olika metoder för att läsa olika typer av textfiler, till exempel kan du använda fgetcsv() funktionen för att läsa en CSV-fil eller DOMDocument klassen för att läsa en XML-fil.

Att läsa och hantera textfiler är en viktig del av programmering och det är bra att ha en grundläggande kunskap om filinläsning för att kunna utveckla mer komplexa program i framtiden.

##Se även

- [PHP manual för fopen()](https://www.phpne