---
title:                "Å få dagens dato"
html_title:           "PHP: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

Hva og hvorfor?
Det å få datoen er en måte for programmerere å få informasjon om den nåværende datoen. Dette kan være nyttig for å sjekke om en bestemt hendelse har skjedd eller for å planlegge fremtidige operasjoner.

Hvordan:
For å få den nåværende datoen i PHP, kan du bruke funksjonen date (). Dette vil returnere datoen og tidspunktet i et gitt format. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
<?php
echo date("d.m.Y");
```
Dette vil gi følgende utdata: 23.09.2021.

Hvis du vil formatere datoen på en annen måte, kan du bruke ulike parametere i date () -funksjonen. For eksempel, hvis du bare ønsker å få måneden, kan du bruke følgende kode:

```PHP
<?php
echo date("m");
```
Dette vil gi den nåværende måneden som utdata, for eksempel 09 for september.

Du kan også legge til tekst og symboler i formatet ditt. For eksempel, hvis du ønsker å få datoen på formatet "dd.mm.yyyy", kan du bruke følgende kode:

```PHP
<?php
echo date("d.m.Y");
```

Dybde på dykk:
Å få datoen med PHP er enkelt og en vanlig operasjon i programmering. Tidligere, før PHP 4.3, måtte utviklere bruke funksjonene gettimeofday () eller mktime () for å få den nåværende datoen. Men med introduksjonen av date () -funksjonen, ble prosessen enklere og mer effektiv.

Det finnes også alternativer til date () -funksjonen, som for eksempel DateTime-klassen som ble introdusert i PHP 5.2. Dette alternativet gir mer fleksibilitet og funksjonalitet for å jobbe med datoer og tider.

Implementeringsdetaljer om å få datoen i PHP kan variere avhengig av versjonen du bruker. Det er alltid lurt å sjekke PHP-dokumentasjonen for å være sikker på at du bruker den riktige syntaksen og parameterne.

Se også:
- Offisiell PHP-dokumentasjon om date () -funksjonen: https://www.php.net/manual/en/function.date.php
- Dokumentasjon for DateTime-klassen: https://www.php.net/manual/en/class.datetime.php
- Mer info om PHP og datoer: https://www.w3schools.com/php/php_date.asp