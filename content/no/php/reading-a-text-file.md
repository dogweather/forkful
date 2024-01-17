---
title:                "Lese en tekstfil"
html_title:           "PHP: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?

Lesing av en tekstfil i PHP er en viktig del av programmering. Dette gjøres for å kunne få tilgang til og bearbeide informasjon lagret i en tekstfil. Tekstfiler er en enkel og vanlig måte å lagre data på, og derfor er det viktig å vite hvordan man kan lese dem i sine programmer.

# Hvordan:

For å lese en tekstfil i PHP, kan man bruke funksjonen `file_get_contents()`. Eksempelvis:

```PHP
$file_content = file_get_contents("tekstfil.txt"); // Leser hele innholdet av tekstfilen og lagrer det i variabelen $file_content
echo $file_content; // Skriver ut innholdet av tekstfilen i nettleseren
```

Når man kjører dette eksempelet, vil innholdet av tekstfilen "tekstfil.txt" bli skrevet ut i nettleseren.

Man kan også lese en tekstfil linje for linje ved å bruke en løkke og funksjonen `fgets()`. Eksempelvis:

```PHP
$myfile = fopen("tekstfil.txt", "r"); // Åpner tekstfilen i lesertilgang
while(!feof($myfile)){ // Løkke som går gjennom hele filen til den når slutten
  echo fgets($myfile) . "<br>"; // Skriver ut én linje av gangen, med en linjeskift mellom hver
}
fclose($myfile); // Lukker filen når løkken er ferdig
```

Når dette eksempelet kjøres, vil innholdet av tekstfilen bli skrevet ut linje for linje i nettleseren.

# Dykk dypere:

Lesing av tekstfiler har vært en viktig del av programmering siden starten av webutvikling. Før PHP ble utviklet, ble det vanligvis gjort ved hjelp av programmeringsspråket Perl. I dag er lesing av tekstfiler i PHP en svært effektiv og vanlig måte å håndtere data på.

En alternativ måte å lese tekstfiler på i PHP, er ved hjelp av funksjonen `file()`. Denne funksjonen leser hele filen og legger hver linje inn i et array. Dette kan være en enklere måte å håndtere data på, spesielt hvis man bare trenger å jobbe med én linje av gangen.

Det er viktig å merke seg at når man leser en tekstfil i PHP, vil hele innholdet av filen bli lagt inn i minnet. Dette kan påvirke ytelsen til nettsiden hvis filen er stor. Derfor er det lurt å begrense lesing av tekstfiler til små filer, og vurdere andre metoder for å håndtere store datamengder.

# Se også:

For mer informasjon om hvordan man kan lese tekstfiler i PHP, anbefales det å se på følgende ressurser:

- [Official PHP documentation for file handling functions](https://www.php.net/manual/en/ref.filesystem.php)
- [W3Schools tutorial on reading files in PHP](https://www.w3schools.com/php/php_file_open.asp)
- [GeeksforGeeks article on different ways to read a file in PHP](https://www.geeksforgeeks.org/php-program-to-find-the-length-of-the-longest-word-in-a-file/)