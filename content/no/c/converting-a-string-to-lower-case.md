---
title:    "C: Konvertere en streng til små bokstaver."
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å ville konvertere en streng til små bokstaver i et C-program. Det kan for eksempel være for å forenkle sammenligning av tekster eller for å følge standard konvensjoner for tekstformattering.

## Hvordan
For å konvertere en streng til små bokstaver i C, kan vi bruke funksjonen `tolower()` fra standardbiblioteket `ctype.h`. Vi kan også skrive vår egen funksjon som itererer gjennom strengen og endrer hvert tegn til en liten bokstav ved hjelp av ASCII-kodene for hver bokstav.

```C
#include <stdio.h>
#include <ctype.h>

// Funksjon for å konvertere en streng til små bokstaver ved hjelp av tolower()
void toLowerCaseUsingToLower(char str[]) {
	for (int i = 0; str[i] != '\0'; i++) {
    	str[i] = tolower(str[i]);
    }
    printf("Konvertert streng ved hjelp av tolower(): %s", str);
}

// Funksjon for å konvertere en streng til små bokstaver ved hjelp av ASCII-koder
void toLowerCaseUsingASCII(char str[]) {
	for (int i = 0; str[i] != '\0'; i++) {
    	if (str[i] >= 'A' && str[i] <= 'Z') {
        	str[i] = str[i] + 32;
        }
    }
    printf("Konvertert streng ved hjelp av ASCII-koder: %s", str);
}

int main() {
	char str[] = "Dette ER en STRENG";
	
	toLowerCaseUsingToLower(str);
	// Output: Konvertert streng ved hjelp av tolower(): dette er en streng
	
	toLowerCaseUsingASCII(str);
	// Output: Konvertert streng ved hjelp av ASCII-koder: dette er en streng
	
	return 0;
}
```

## Dykk dypere
Når vi bruker `tolower()`-funksjonen, konverteres tegnene kun hvis de er store bokstaver. Alle andre tegn, som tall og symboler, forblir uendret. Dette kan føre til uventede resultater hvis vi ønsker å konvertere en hel streng til små bokstaver. Derfor kan det være lurt å kombinere `tolower()` med en løkke som sjekker og behandler hvert tegn i strengen.

## Se også
- [The C Programming Language](https://www.amazon.com/Programming-Language-Brian-W-Kernighan/dp/0131103628)
- [C Strings](https://www.programiz.com/c-programming/c-strings)
- [tolower() function in C](https://www.geeksforgeeks.org/tolower-function-in-c/)