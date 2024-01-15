---
title:                "Skapa en temporär fil"
html_title:           "PHP: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil är en vanlig praxis inom programmering. Det kan vara användbart för att hantera data som inte behöver sparas permanent eller för att utföra tillfälliga operationer.

## Så här gör du

För att skapa en temporär fil i PHP finns det en inbyggd funktion som heter "tempnam()". Den använder ett systemgenererat namn och returnerar sökvägen till den nya filen. Exempel:

```PHP
$temp_file = tempnam(sys_get_temp_dir(), 'prefix_'); // Skapar en ny temporär fil
echo "Sökväg till temporär fil: " . $temp_file; // Exempeloutput: Sökväg till temporär fil: /tmp/prefix_ztjKjC 
```

En annan alternativ funktion är "tmpfile()", som skapar en temporär fil och returnerar en öppen filressurs som används för att skriva till filen. Exempel:

```PHP
$temp_file = tmpfile(); // Skapar en ny temporär fil
fwrite($temp_file, "Detta är en temporär fil."); // Skriver till filen
rewind($temp_file); // Återgår till början av filen
echo "Innehåll i temporär fil: " . fread($temp_file, filesize($temp_file)); // Exempeloutput: Innehåll i temporär fil: Detta är en temporär fil.
```

## Djupdykning

När man skapar en temporär fil är det viktigt att tänka på säkerheten. Eftersom filen inte kommer att användas permanent kan den bli kvar på servern och bli en säkerhetsrisk om den inte hanteras på rätt sätt. För att undvika detta kan man använda sig av en "cleanup" funktion, som tar bort den temporära filen när den inte längre behövs. Exempel:

```PHP
$temp_file = tempnam(sys_get_temp_dir(), 'prefix_');
// Gör något med filen...
unlink($temp_file); // Rensar upp filen när den inte längre behövs
```

En annan viktig aspekt att tänka på är att temporära filer kan ta upp en stor mängd serverutrymme om de inte rensas upp regelbundet. Det är därför viktigt att ha en mekanism för att ta bort temporära filer som inte längre används.

## Se även

- [PHP: tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [PHP: tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)