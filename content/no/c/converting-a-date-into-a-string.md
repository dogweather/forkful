---
title:                "C: Konvertering av en dato til en streng"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å konvertere en dato til en streng i en C-programmering. Noen ganger trenger man kanskje å formatere datoen for å vise den på en bestemt måte, eller kanskje man ønsker å lagre datoen som en del av en tekststreng. Uansett årsak, er dette en viktig ferdighet å ha når man skriver dataprogrammer.

## Hvordan

For å konvertere en dato til en streng, må man følge noen enkle trinn. Først må man definere en variabel av typen "struct tm", som representerer datoen. Deretter bruker man "strptime" funksjonen for å konvertere en streng til et "tm" objekt. Til slutt bruker man "strftime" funksjonen til å formatere datoen til en streng, og lagrer den i en annen variabel.

```C
// Definer en variabel for datoen
struct tm mydate;

// Bruk strptime til å konvertere en streng til en "tm" struct
strptime("01/01/2020", "%m/%d/%Y", &mydate);

// Definer en buffer for den konverterte datoen
char buffer[80];

// Bruk strftime til å formatere datoen og lagre den i bufferen
strftime(buffer, 80, "%d %B, %G", &mydate);

// Skriv ut resultatet
printf("Dato som streng: %s\n", buffer);

```

For dette eksempelet, vil output være "01 January, 2020".

## Dypdykk

Når man konverterer en dato til en streng, er det viktig å være bevisst på hvilke format man ønsker å bruke. Det finnes forskjellige formater for datoer, som kan variere fra land til land eller bransje til bransje. Det er derfor viktig å sjekke hvilke standarder som gjelder for det programmet eller systemet man jobber med.

Videre er det også viktig å huske at datoer kan være kompliserte, spesielt når det kommer til måneder og dager. For eksempel har ikke alle måneder like mange dager, og det kan være skuddår som må tas hensyn til. Derfor er det viktig å være nøyaktig i hvordan man konverterer og formaterer datoer for å unngå feil.

## Se også

- [C Strptime](https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm)
- [C Strftime](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Date and Time Functions in C](https://www.programiz.com/c-programming/c-date-time-functions)