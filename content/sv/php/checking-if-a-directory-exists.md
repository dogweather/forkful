---
title:                "PHP: Kontrollera om en mapp existerar"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av PHP-programmering. Det är en enkel men kraftfull funktion som hjälper till att säkerställa att våra program fungerar korrekt och ger rätt resultat. Detta är särskilt viktigt när man arbetar med stora och komplexa projekt, där det kan finnas flera olika mappar som ska hanteras.

## Hur man gör det

För att kontrollera om en mapp existerar i PHP kan du använda funktionen `is_dir()`. Detta kommer att returnera ett booleskt värde (true eller false) beroende på om mappen finns eller inte.

```PHP 
<?php 
// Definiera sökvägen till mappen som ska kontrolleras 
$folder = "/hem/min_mapp"; 

// Använd is_dir() för att se om mappen existerar 
if(is_dir($folder)) { 
  echo "Mappen existerar."; 
} else { 
  echo "Mappen existerar inte."; 
} 
?> 
```

Beroende på om mappen finns eller inte kommer koden att skriva ut en av meddelandena. Om mappen `min_mapp` existerar kommer det första meddelandet att skrivas ut, annars kommer det andra meddelandet att skrivas ut.

## Djupdykning

För mer avancerade projekt kan det vara användbart att veta mer om mappen som ska kontrolleras. I så fall kan funktionen `scandir()` användas för att returnera en lista över filer och mappar inuti den specifika mappen.

```PHP 
<?php 
// Definiera sökvägen till mappen som ska kontrolleras 
$folder = "/hem/min_mapp"; 

// Använd scandir() för att lista alla filer och mappar 
$contents = scandir($folder); 

// Skriv ut resultatet 
print_r($contents); 
?> 
```

Koden ovan kommer att skriva ut en lista med alla filer och mappar som finns i mappen `min_mapp`. Detta kan vara användbart för att kontrollera om rätt filer eller mappar finns innan man fortsätter med resten av koden.

## Se även

- [PHP Manual: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Manual: scandir()](https://www.php.net/manual/en/function.scandir.php)