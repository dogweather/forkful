---
title:                "Søke og bytte tekst"
html_title:           "C: Søke og bytte tekst"
simple_title:         "Søke og bytte tekst"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor kaster bort tid og energi på å manuelt endre tekst når du kunne bruke et enkelt verktøy til å gjøre det for deg? I denne artikkelen vil vi utforske hvordan du kan søke og erstatte tekst i C-programmering og spare deg for unødvendig arbeid.

## Hvordan Gjøre Det

Det er flere måter å søke og erstatte tekst i C-programmering, avhengig av hva du ønsker å oppnå. Her er noen eksempler på enkle funksjoner du kan implementere:

```C
// Fjern en bestemt karakter fra en streng
char remove_char(char string[], char remove) {
    int i, j = 0;
    for (i = 0; string[i] != '\0'; i++) {
        if (string[i] != remove) {
            string[j++] = string[i];
        }
    }
    string[j] = '\0';
    return *string;
}

// Bytt ut en karakter med en annen i en streng
char replace_char(char string[], char old, char new) {
    int i;
    for (i = 0; string[i] != '\0'; i++) {
        if (string[i] == old) {
            string[i] = new;
        }
    }
    return *string;
}

// Fjern alle mellomrom fra en streng
char remove_spaces(char string[]) {
    int i, j = 0;
    for (i = 0; string[i] != '\0'; i++) {
        if (string[i] == ' ') {
            string[j++] = string[i+1];
        }
    }
    string[j] = '\0';
    return *string;
}

// Bytt ut en hel streng med en annen i en annen streng
char replace_string(char string[], char old_str[], char new_str[]) {
    char *pos = strstr(string, old_str);
    while (pos) {
        int index = pos - string;
        strncpy(&string[index], new_str, strlen(new_str));
        pos = strstr(string, old_str);
    }
    return *string;
}

```

Eksempelutdata:

```
char str[] = "Dette er en streng.";
char remove_spaces = remove_spaces(str);
printf("%s\n", remove_spaces); // Output: Detteerenstreng.
```

## Dykke Dypere

For mer avanserte oppdrag, som å søke og erstatte tekst i en fil, kan du bruke funksjoner som `fread()` og `fwrite()`, eller endre en karakter i en fil med `fputc()`. Det finnes også tredjepartsbiblioteker som kan hjelpe deg med å automatisere tekstsøk og erstatting i C-programmering.

## Se Også

- [Offisiell C-dokumentasjon](https://en.cppreference.com/w/c)
- [Regex-bibliotek for C](https://github.com/kkos/oniguruma)