---
title:                "PHP: Å få den nåværende datoen"
simple_title:         "Å få den nåværende datoen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Uansett om du utvikler en nettside eller et program, er det ofte nyttig å vise den nåværende datoen til brukeren. Dette kan hjelpe med å organisere informasjon og sørge for at den er oppdatert. I denne artikkelen vil vi utforske hvordan du kan få tak i den nåværende datoen ved hjelp av PHP-programmering.

## Slik gjør du det
For å få tak i dagens dato i PHP, kan du bruke funksjonen `date()`. Denne funksjonen tar imot en formatstreng som parameter og returnerer datoen i det ønskede formatet. La oss se på et eksempel:

```PHP
$dato = date("d.m.Y");
echo "Dagens dato er " . $dato;
// Resultat: Dagens dato er 09.02.2021
```

I dette tilfellet brukte vi formatstrengen "d.m.Y" for å få datoen til å bli vist som "dag.måned.år". Det finnes en rekke ulike formatstrenger du kan bruke, avhengig av hvordan du ønsker å vise datoen. For eksempel kan du bruke "l, d. F Y" for å få datoen til å bli vist som "tirsdag, 09. februar 2021".

Du kan også bruke funksjonen `mktime()` for å få tak i en spesifikk dato. Denne funksjonen tar imot argumenter for år, måned og dag, og returnerer datoen i Unix-tidsformat. Her er et eksempel:

```PHP
$dato = mktime(0, 0, 0, 12, 24, 2021);
echo "Julen 2021 er på en " . date("l", $dato);
// Resultat: Julen 2021 er på en fredag
```

I dette tilfellet brukte vi `date("l")` for å få datoen til å bli vist som en ukedag, basert på Unix-tidsformatet vi fikk fra `mktime()`.

## Dypdykk
Hvis du ønsker å få tak i mer avansert informasjon om datoen, kan du bruke funksjonen `getdate()`. Denne funksjonen tar imot en Unix-tidskode som parameter og returnerer en assosiativt array med detaljert informasjon om datoen. La oss se på et eksempel:

```PHP
$dato = getdate();
print_r($dato);
// Resultat:
// Array (
//    [seconds] => 17
//    [minutes] => 37
//    [hours]   => 12
//    [mday]    => 09
//    [wday]    => 2
//    [mon]     => 2
//    [year]    => 2021
//    [yday]    => 39
//    [weekday] => tirsdag
//    [month]   => februar
//    [0]       => 1612875437
// )
```

Som du kan se, returnerer `getdate()` en rekke ulike verdier, som for eksempel sekunder, minutter, timer, dag i måneden, dag i uken osv. Du kan også bruke denne funksjonen til å få tak i informasjon om en spesifikk dato ved å spesifisere en Unix-tidskode som parameter.

## Se også
- [PHP.net - date()](https://www.php.net/manual/en/function.date.php)
- [PHP.net - mktime()](https://www.php.net/manual/en/function.mktime.php)
- [PHP.net - getdate()](https://www.php.net/manual/en/function.getdate.php)