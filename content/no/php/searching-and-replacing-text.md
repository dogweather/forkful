---
title:                "Søking og Erstating av Tekst"
html_title:           "PHP: Søking og Erstating av Tekst"
simple_title:         "Søking og Erstating av Tekst"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Hvorfor:

Å søke og erstatte tekst er en vanlig oppgave for både nybegynnere og erfarne PHP-programmerere. Det lar deg enkelt endre store mengder tekst på en rask og effektiv måte, noe som sparer deg for mye tid og frustrasjon. Enten du trenger å rette opp skrivefeil eller utføre en stor omformatering av kode, er søking og erstattning et viktig verktøy å ha på plass.

Hvordan:

Søking og erstattning i PHP er enkelt og innebærer bruk av både en søkestreng og en erstatningsstreng. Den grunnleggende syntaksen er som følger:

```PHP
str_replace(søkestreng, erstatningsstreng, tekst);
```

La oss si at du har en variabel som heter "tekst" med følgende innhold:

```PHP
$tekst = "Velkommen til min nettside!";
```

Hvis du ønsker å endre "Velkommen" til "Hei", kan du gjøre dette ved å bruke følgende kode:

```PHP
$ny_tekst = str_replace("Velkommen", "Hei", $tekst);
```

Den nye variabelen "ny_tekst" vil nå inneholde følgende:

```PHP
$ny_tekst = "Hei til min nettside!";
```

Som du kan se, har "Velkommen" nå blitt erstattet med "Hei" i variabelen "ny_tekst". Du kan også bruke søke- og erstatningsfunksjonen på vanlige tekststrenger, for eksempel:

```PHP
$ny_tekst = str_replace("Min", "Din", $tekst);
```

Denne koden vil erstatte "Min" med "Din", og den nye variabelen vil se slik ut:

```PHP
$ny_tekst = "Velkommen til din nettside!";
```

Hvis du ønsker å søke og erstatte i en hel liste med tekststrenger, kan du også bruke en foreach-løkke. For eksempel:

```PHP
$tekst_liste = array("Lorem", "Ipsum", "Dolor");

foreach($tekst_liste as $tekst){
    $ny_tekst = str_replace("o", "a", $tekst);
    echo $ny_tekst . ",";
}
```

Dette vil erstatte alle forekomster av bokstaven "o" med "a" i hvert element i listen, og vil resultere i følgende output:

```
Larem, Ipsam, Dalor
```

Dykk dypere:

Det er også mulig å bruke regulære uttrykk i søke- og erstatningsfunksjonen for mer avansert søking og erstatning. Dette vil være nyttig hvis du for eksempel ønsker å erstatte alle tall med et annet tall, eller hvis du ønsker å søke etter et bestemt mønster i teksten din.

For å bruke regulære uttrykk, må du legge til en "e" ved slutten av erstatningsstrengen. La oss si at du ønsker å erstatte alle tall i en tekststreng med 0, kan du gjøre dette ved å bruke følgende kode:

```PHP
$tekst = "123-456-789";
$ny_tekst = preg_replace("/[0-9]+/", "0", $tekst);
```

Denne koden vil resultere i følgende output:

```PHP
$ny_tekst = "0-0-0";
```

Som du kan se, har alle tall blitt erstattet med 0 ved hjelp av dette regulære uttrykket.

Se også:

- Offisiell PHP-dokumentasjon for str_replace() funksjonen: https://www.php.net/manual/en/function.str-replace.php
- Offisiell PHP-dokumentasjon for preg_replace() funksjonen: https://www.php.net/manual/en/function.preg-replace.php
- En enkel guide til regulære uttrykk i PHP: https://www.php.net/manual/en/regexp.reference.php