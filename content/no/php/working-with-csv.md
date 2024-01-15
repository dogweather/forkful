---
title:                "Arbeide med csv"
html_title:           "PHP: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-csv.md"
---

{{< edit_this_page >}}

# Hvorfor

CSV (Comma Separated Values) er en vanlig filformat som brukes for å lagre og utveksle data. Det er en enkel og effektiv måte å organisere og lagre store mengder data på. Derfor er det viktig å forstå hvordan man kan håndtere og jobbe med CSV-filer i PHP.

# Hvordan

For å begynne å jobbe med CSV-filer i PHP, må du først åpne filen ved hjelp av `fopen()` funksjonen. Deretter må du bruke `fgetcsv()` funksjonen for å lese innholdet i filen og konvertere det til et array. Dette lar deg enkelt få tilgang til hver enkelt linje og data i CSV-filen.

```PHP
$file = fopen("file.csv", "r"); // Åpner CSV-fil for lesing
while (($line = fgetcsv($file)) !== false) { // Leser hver linje i filen
    print_r($line); // Skriver ut linjen som et array
}
```

Du kan også spesifisere et spesifikt skilletegn for å lese filen, for eksempel hvis det ikke bruker komma som standard. Dette gjøres ved å legge til et ekstra argument i `fgetcsv()` funksjonen.

For å legge til data i en CSV-fil, bruker du `fputcsv()` funksjonen. Dette konverterer et array til CSV-format og legger det til i filen.

```PHP
$data = array("Name", "Age", "Country"); // Oppretter et array med data
$file = fopen("file.csv", "a"); // Åpner CSV-fil for appending (legg til på slutten av filen)
fputcsv($file, $data); // Legger til arrayet som en linje i filen
```

# Deep Dive

Det er viktig å merke seg at CSV-filer kan være så enkle eller komplekse som du ønsker. Det er ingen begrensninger på antall kolonner eller rader som kan inkluderes i en CSV-fil. Det er heller ingen standard for datatype, så du må være forsiktig når du håndterer data for å unngå problemer.

En vanlig feil når man arbeider med CSV-filer er å glemme å ta hensyn til sitattegn og kvalifisering av data. Hvis dataene inneholder komma eller spesialtegn, må de plasseres innenfor sitatfelter for å unngå at de blir tolket feil. Dette kan enkelt gjøres ved å bruke `fputcsv()` funksjonen som vi nevnt tidligere.

Det er også viktig å sørge for at du lukker CSV-filen etter at du har ferdig med å jobbe med den. Du kan bruke `fclose()` funksjonen for å gjøre dette. Dette frigjør ressursene som brukes av filen og sikrer at den er klar til å åpnes igjen hvis nødvendig.

# Se også

- [PHP-funksjoner for håndtering av CSV-filer](https://www.php.net/manual/en/ref.filesystem.php)
- [CSV filformat](https://en.wikipedia.org/wiki/Comma-separated_values)