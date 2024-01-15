---
title:                "Sjekke om en mappe eksisterer"
html_title:           "PHP: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer i et PHP-program fordi det kan påvirke programflyten og forhindre feil hvis mappen ikke finnes. Det er også nyttig for å håndtere forskjellige operativsystemer som har forskjellige måter å behandle mapper på.

## Hvordan

For å sjekke om en mappe eksisterer i et PHP-program, kan du bruke funksjonen `is_dir()` sammen med mappen som parameter. Hvis mappen eksisterer, vil funksjonen returnere `true`, ellers vil den returnere `false`. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
$mappe = "/hjem/bruker/dokumenter/";

if(is_dir($mappe)){
  echo "Mappen eksisterer!";
} else {
  echo "Mappen eksisterer ikke!";
}
```

Outputen av dette eksempelet vil avhenge av om `dokumenter`-mappen faktisk eksisterer eller ikke. Hvis den eksisterer, vil du se `Mappen eksisterer!` i nettleseren din, ellers vil du få `Mappen eksisterer ikke!`.

## Dypdykk

I tillegg til å bruke `is_dir()`-funksjonen, kan du også bruke `file_exists()`-funksjonen for å sjekke om en mappe eksisterer. Forskjellen mellom disse to funksjonene er at `file_exists()` også kan brukes til å sjekke om filer eksisterer, mens `is_dir()` bare sjekker om det er en mappe.

En annen viktig ting å vurdere når du sjekker om en mappe eksisterer er stien du bruker. Hvis du bruker en absolutt sti, må du være sikker på at den er riktig for operativsystemet du kjører programmet på. Du kan også bruke en relativ sti, men da må du huske å ta hensyn til hvor programmet ditt er plassert i forhold til mappen du sjekker.

## Se Også

- [PHP Manual: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Manual: file_exists()](https://www.php.net/manual/en/function.file-exists.php)