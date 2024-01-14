---
title:                "PHP: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man programmerer i PHP, kan det hende at man trenger å sjekke om en mappe eksisterer før man utfører en spesifikk handling. Dette kan være for å unngå feil, sikre at koden kjører som den skal, eller for å gi relevant informasjon til brukeren.

## Hvordan

Det finnes flere måter å sjekke om en mappe eksisterer i PHP, men den mest effektive metoden er å bruke funksjonen `file_exists()`. Denne funksjonen tar en filbane som argument og returnerer `true` hvis filen eller mappen eksisterer, eller `false` hvis den ikke finnes.

```PHP
$mappe = 'bilder/';

if(file_exists($mappe)){
    echo "Mappen eksisterer!";
}
else{
    echo "Mappen eksisterer ikke.";
}
```

I dette eksempelet brukes en if-else statement for å sjekke om mappen eksisterer. Hvis mappen eksisterer, vil en melding bli vist til brukeren, ellers vil en annen melding vises.

## Dypdykk

Det er viktig å merke seg at `file_exists()` funksjonen også kan brukes til å sjekke om en fil eksisterer. Denne funksjonen vil imidlertid også returnere `true` hvis filen du sjekker for eksistens er en tom fil. For å unngå dette, kan man også bruke funksjonen `is_dir()`, som kun returnerer `true` hvis det faktisk er en mappe som eksisterer.

```PHP
$fil = 'dokumenter/ferieplan.docx';

if(file_exists($fil)){
    if(is_dir($fil)){
        echo "Dette er en mappe.";
    }
    else{
        echo "Dette er en fil.";
    }
}
else{
    echo "Filen eksisterer ikke.";
}
```

Dette eksempelet viser hvordan man først sjekker om mappen eller filen eksisterer, og deretter bruker `is_dir()` for å skille mellom en fil og en mappe.

## Se også

- [PHP.net - file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [PHP.net - is_dir()](https://www.php.net/manual/en/function.is-dir.php)