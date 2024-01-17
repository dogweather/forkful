---
title:                "Å jobbe med csv"
html_title:           "PHP: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
CSV står for Comma Separated Values (kommaseparerte verdier) og er et enkelt og vanlig filformat for å lagre og utveksle data. Mange programmerere bruker CSV-filer for å lagre og manipulere store mengder data på en effektiv måte.

# Hvordan:
Bruk PHPs innebygde funksjoner for å håndtere CSV-filer. Først må du åpne en CSV-fil ved å bruke fopen() funksjonen. Deretter kan du bruke fgetcsv() funksjonen til å lese data fra fila linje for linje. Du kan også bruke fputcsv() funksjonen for å skrive data til en CSV-fil.

```PHP
// Åpne en CSV-fil for lesing
$fil = fopen('data.csv', 'r');

// Les data linje for linje
while(($data = fgetcsv($fil)) !== FALSE){
    print_r($data);
}

// Åpne en CSV-fil for skriving
$fil = fopen('data.csv', 'w');

// Skriv data til fila
$data = array('Navn', 'Alder', 'E-post');
fputcsv($fil, $data);

// Lukk filen
fclose($fil);
```

Eksempel:
Lar oss si at du har en CSV-fil med informasjon om ansatte på et selskap. Du kan bruke fgetcsv() funksjonen til å lese data fra fila og deretter bruke denne informasjonen til å opprette en nettleservennlig tabell med alle ansattes navn, alder og e-postadresser.

# Dypdykk:
CSV-filer har eksistert siden 1970-tallet og har vært et populært format for utveksling av data mellom forskjellige programmer. Det finnes alternativer til CSV som XML og JSON, men CSV er fortsatt mye brukt på grunn av sin enkelhet og effektivitet.

Når du jobber med CSV-filer, er det viktig å være oppmerksom på tegnsettet som brukes i fila. Noen ganger kan dette føre til problemer når du skal lese eller skrive data. Det er også viktig å sørge for at fila er formatert riktig, med komma som skilletegn mellom verdiene.

En annen nyttig funksjon i PHP når man jobber med CSV-filer er str_getcsv() som lar deg konvertere en CSV-streng til et array.

# Se også:
- [PHP.net - CSV](https://www.php.net/manual/en/function.fgetcsv.php)
- [Wikipedia - Comma-separated values](https://en.wikipedia.org/wiki/Comma-separated_values)