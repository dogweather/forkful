---
title:    "PHP: Skriva en textfil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

...

## Varför

Att kunna skriva till textfiler är en viktig färdighet för alla PHP-programmerare. Det gör det möjligt att dynamiskt generera och spara innehåll från ett program.

## Hur man gör

För att skriva till en textfil i PHP kan du använda fopen (), fwrite () och fclose () -funktioner. Först måste vi öppna filen för skrivning med fopen ():

````PHP
$file = fopen("mittfilnamn.txt", "w");
````

Sedan kan vi använda fwrite () för att skriva data till filen, till exempel en sträng:

````PHP
fwrite($file, "Det här är en textsträng som kommer att sparas");
````

När vi är klara måste vi stänga filen med fclose ():

````PHP
fclose($file);
````

Detta är en grundläggande kodexempel för att skriva till en textfil. Se till att kontrollera att du har rätt behörighet att skriva till filen innan du kör koden.

## Djupdykning

Om du vill läsa innehållet från en befintlig textfil istället för att bara lägga till nytt innehåll, kan du använda fopen () med läget "r" för läsning:

````PHP
$file = fopen("mittfilnamn.txt", "r");
````

Sedan kan du använda fgets () för att läsa innehållet rad för rad:

````PHP
while(!feof($file)) {
  echo fgets($file). "<br>";
}
````

Du kan också använda fil () -funktionen för att läsa innehållet av en hel fil till en array:

````PHP
$file_content = file("mittfilnamn.txt");
print_r($file_content);
````

Det finns också andra funktioner som gör det möjligt att manipulera textfiler på olika sätt, till exempel att radera eller omstrukturera innehållet. Det är viktigt att läsa på dokumentationen och förstå hur dessa funktioner fungerar innan du använder dem.

## Se även

- [PHP fopen () function](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite () function](https://www.php.net/manual/en/function.fwrite.php)
- [PHP fclose () function](https://www.php.net/manual/en/function.fclose.php)