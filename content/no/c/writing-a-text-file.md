---
title:                "Skrive en tekstfil"
html_title:           "C: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive en tekstfil er en vanlig og nyttig oppgave for de som jobber med programmering. Det er en enkel og effektiv måte å lagre og organisere data på, og kan brukes til en rekke formål som å lagre brukerinnstillinger, loggfiler og annen viktig informasjon. Ved å skrive en tekstfil, kan du enkelt lagre og gjenopprette data når det er nødvendig.

## Hvordan
For å skrive en tekstfil i C, må du følge noen enkle trinn. Først må du åpne en filstrøm ved hjelp av ```fopen```-funksjonen, og gi navnet til filen du vil skrive til og modusen du vil åpne filen i. For eksempel, hvis du vil åpne en fil for å skrive til den, kan du bruke følgende kode:

```C
FILE *fp;
fp = fopen("minfil.txt", "w");
```

Deretter kan du bruke ```fprintf```-funksjonen til å skrive data til filen. Dette kan gjøres ved å bruke et spesifikt format, for eksempel ```%d``` for heltall og ```%s``` for strenger. For å legge til en ny linje i filen, kan du bruke ```\n```. Her er et eksempel på å skrive en tekststreng til filen:

```C
char *tekst = "Dette er en tekststreng";
fprintf(fp, "%s\n", tekst);
```

Til slutt, må du huske å lukke filstrømmen ved hjelp av ```fclose```-funksjonen når du er ferdig med å skrive til filen.

## Dypdykk
Nå som du vet hvordan du kan skrive en tekstfil i C, kan det være nyttig å vite litt mer om noen av de andre funksjonene og modusene som kan brukes. For eksempel, hvis du vil legge til data til en eksisterende fil istedenfor å overskrive den, kan du bruke modusen "a" i ```fopen```-funksjonen. Hvis du vil lese en tekstfil, kan du bruke ```fscanf```-funksjonen på samme måte som ```fprintf```-funksjonen. Det finnes også andre nyttige funksjoner som ```rewind``` for å gå tilbake til starten av filen og ```ftell``` for å finne posisjonen til lese-/skrivepekeren i filen.

En annen viktig ting å merke seg er at når du skriver en tekstfil, så blir den lagret som en ren tekstfil uten noe form for formatering. Dette betyr at du ikke kan lagre variabler eller datastrukturer direkte til filen. I stedet må du konvertere dataene til tekststrenger før du skriver dem til filen, og så konvertere dem tilbake når du leser dem.

## Se også
- [C programmering – Wikibooks](https://no.wikibooks.org/wiki/C_programmering)
- [File Input/Output i C – TutorialsPoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-biblioteket – CppReference](https://en.cppreference.com/w/c/io)