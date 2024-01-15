---
title:                "Last ned en nettside"
html_title:           "C: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å laste ned en nettside. Kanskje du vil lagre informasjonen for senere bruk, eller kanskje du vil endre nettsiden og laste den opp igjen. Uansett årsak er det nyttig å vite hvordan man kan laste ned en nettside ved hjelp av C-programmering.

## Hvordan gjøre det
For å laste ned en nettside ved hjelp av C, trenger du først å inkludere standardbiblioteket `stdio.h` og `stdlib.h` i koden din. Deretter må du initialisere en variabel med `fopen` funksjonen for å åpne en kobling til nettsiden du ønsker å laste ned. Deretter bruker du `fputc` funksjonen til å skrive innholdet i nettsiden til en fil. Du kan velge å lese innholdet i nettsiden linje for linje, eller ved hjelp av `getc` funksjonen.

```C
#include<stdio.h>
#include<stdlib.h>

int main(){
    FILE *fptr;
    int ch;
    fptr = fopen("nettside.txt","w"); // Åpner en ny fil kalt nettside.txt
    FILE *nettsiden = fopen("https://www.example.com", "r"); // Åpner nettsiden som en lesbar fil
    if(nettsiden == NULL) {
        printf("Kan ikke åpne nettsiden");
        exit(1);
    }
    // Leser og lagrer nettsiden i en fil
    while((ch = getc(nettsiden)) != EOF) {
        fputc(ch, fptr);
    }
    fclose(nettsiden);
    fclose(fptr);
}
```

Dette vil skrive innholdet fra nettsiden til en fil som heter `nettside.txt`. Du kan også bruke `fgets` funksjonen til å lese og skrive en bestemt del av nettsiden.

## Dybdeundersøkelse
Ved å dykke dypere inn i C-programmering, kan man oppdage flere avanserte måter å laste ned en nettside på. For eksempel kan man bruke `libcurl` biblioteket for å hente nettsiden ved hjelp av HTTP-protokoller. Man kan også bruke `getaddrinfo` funksjonen for å hente IP-adressen til nettsiden og åpne forbindelse direkte til serveren.

## Se også
- [Libcurl dokumentasjon](https://curl.haxx.se/libcurl/)
- [getaddrinfo dokumentasjon](https://man7.org/linux/man-pages/man3/getaddrinfo.3.html)