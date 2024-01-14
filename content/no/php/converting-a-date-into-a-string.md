---
title:                "PHP: Konvertere en dato til en streng."
simple_title:         "Konvertere en dato til en streng."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være en nyttig ferdighet for å formatere datoer i et leselig format eller for å sammenligne datoer i et program. Dette gjør det også enklere å vise datoer på ulike språk og i ulike formater.

## Hvordan

```PHP
$today = date_create(); // lager en ny instans av "DateTime" 
echo date_format($today, 'd/m/Y'); // skriver ut dag, måned og år på norsk format (01/01/2021)
```

```PHP
$dato = date_create('2020-12-25'); // lager en DateTime instans med en spesifikk dato
setlocale(LC_TIME, "no_NO"); // setter ønsket norsk språk
echo strftime('%A, %d.%B %Y', date_timestamp_get($dato)); // skriver ut ukedag, dag og måned på norsk (fredag, 25.desember 2020)
```

## Dykk dypere

I PHP er det flere funksjoner for å formatere datoer, som `date()` og `strtotime()`. Men ved å bruke `$format` parameteren for `date_format()` eller `strftime()`, kan man få en mer pålitelig og fleksibel måte å konvertere en dato til en streng på. Man kan også bruke `setlocale()` for å få utdato og månedsnavn på ønsket språk.

## Se også

- [PHP.net: Date and Time functions](https://www.php.net/manual/en/ref.datetime.php)
- [W3Schools: PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [GeeksforGeeks: Convert date to string](https://www.geeksforgeeks.org/php-date-time-datetime-datenow-datetime-create-from-format-functions)