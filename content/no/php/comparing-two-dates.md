---
title:    "PHP: Sammenligne to datoer"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, i programmering, må vi sammenligne to datoer for å se om de er like eller om en kommer før eller etter den andre. Dette kan være nyttig for å sortere data eller for å sjekke for utløpsdatoer. I dette blogginnlegget vil vi se på hvordan du kan sammenligne to datoer i PHP-kode.

## Hvordan

For å sammenligne to datoer i PHP, bruker vi funksjonen "strtotime" for å konvertere datoen til en tidssstempel og deretter sammenligner vi de to tidstemplene.

```PHP
$dato1 = "2021-06-01";
$dato2 = "2021-06-10";

$tid1 = strtotime($dato1);
$tid2 = strtotime($dato2);

if($tid1 < $tid2){
    echo "Dato 1 er før Dato 2";
} elseif($tid1 > $tid2){
    echo "Dato 1 er etter Dato 2";
} else{
    echo "Dato 1 og Dato 2 er like";
}
```
**Output:**

*Dato 1 er før Dato 2*

Vi kan også bruke operatorer som "<", ">", eller "==" direkte på datoene, men dette kan føre til uforutsette resultater, spesielt hvis datoene er på forskjellige formater. Derfor anbefales det å bruke tidstempel sammenligning som beskrevet over.

## Dykk dypere

Å sammenligne to datoer kan være mer komplekst enn det vi har sett på i eksemplet over. Dette skyldes forskjellige datoformater, tidszoner og skuddår. For å håndtere dette, anbefales det å bruke PHPs "DateTime" objekt.

```PHP
$dato1 = new DateTime("2021-06-01");
$dato2 = new DateTime("2021-06-10");

if($dato1 < $dato2){
    echo "Dato 1 er før Dato 2";
} elseif($dato1 > $dato2){
    echo "Dato 1 er etter Dato 2";
} else{
    echo "Dato 1 og Dato 2 er like";
}
```

**Output:**

*Dato 1 er før Dato 2*

Med "DateTime" objektet trenger vi ikke å bekymre oss for forskjellige formater og tidszoner, da dette blir håndtert automatisk.

## Se også

- [PHP.net - DateTime](https://www.php.net/manual/en/class.datetime.php)
- [W3Schools - PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [Stack Overflow - How to compare two dates in PHP](https://stackoverflow.com/questions/56177234/how-to-compare-two-dates-and-time-in-php)

Takk for at du leste dette blogginnlegget om å sammenligne to datoer i PHP. Vi håper det har vært nyttig og at det vil hjelpe deg i dine fremtidige programmeringsoppgaver. Lykke til!