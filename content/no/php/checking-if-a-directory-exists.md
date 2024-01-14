---
title:    "PHP: Sjekke om en mappe eksisterer"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har programmert i PHP, har du sannsynligvis støtt på situasjoner der du må sjekke om en mappe eksisterer før du fortsetter kjøringen av koden din. Dette kan virke som en unødvendig sjekk, men det kan være viktig for å sikre at koden din fungerer riktig og forhindre eventuelle feil underveis.

# Hvordan gjøre det

Det er flere måter å sjekke om en mappe eksisterer i PHP, men en av de enkleste er å bruke funksjonen `file_exists()`. Denne funksjonen tar inn en filbane som argument og returnerer enten `true` hvis filen eksisterer, eller `false` hvis den ikke gjør det.

```PHP
if (file_exists("/var/www/mapper")) {
    echo "Mappen eksisterer!";
} else {
    echo "Mappen eksisterer ikke!";
}
```

I dette eksempelet bruker vi filbanen `/var/www/mapper`, men du kan bytte den ut med den filbanen du ønsker å sjekke.

# Dypere dykk

Det er også mulig å bruke funksjonen `is_dir()` for å sjekke om en fil er en mappe eller ikke. Denne funksjonen tar også inn en filbane som argument og returnerer `true` hvis det er en mappe, eller `false` hvis det ikke er det.

Det er også viktig å merke seg at begge disse funksjonene bare sjekker om filer eksisterer eller om de er mapper. Det betyr at de ikke tar hensyn til eventuelle tillatelser eller andre faktorer som kan hindre tilgang til mappen.

# Se også

- [Offisiell PHP-dokumentasjon om file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [Offisiell PHP-dokumentasjon om is_dir()](https://www.php.net/manual/en/function.is-dir.php)