---
title:                "Å bruke regulære uttrykk"
html_title:           "PHP: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regex, eller regulære uttrykk, er et kraftig verktøy som brukes i programvareutvikling for å søke og manipulere tekststrenger. Det er nyttig for å finne og behandle bestemte mønstre innenfor en tekst, noe som kan være svært tidsbesparende for utviklere.

## Hvordan:
```PHP
//Søke etter ordet "hei":
$string = "Hei, jeg heter Per.";
$pattern = "/hei/";
if (preg_match($pattern, $string)) {
  echo "Ordet ble funnet!";
} else {
  echo "Ordet ble ikke funnet.";
}
//Output: "Ordet ble funnet!"
```

Regex kan også brukes til å bytte ut deler av en tekststreng:
```PHP
//Bytte ut ordet "Per" med "Ole":
$string = "Hei, jeg heter Per.";
$pattern = "/Per/";
$replacement = "Ole";
echo preg_replace($pattern, $replacement, $string);
//Output: "Hei, jeg heter Ole."
```

## Dypdykk:
Regex ble først utviklet på 1950-tallet av matematikeren Stephen Cole Kleene. Dette verktøyet har vært viktig for å effektivisere tekstbehandling og dataanalyse i flere tiår.

Det finnes også alternative måter å utføre lignende funksjoner på, som for eksempel string-manipuleringsfunksjoner i PHP. Men regex kan være mer fleksibelt og kraftig når man trenger å utføre komplekse søk og erstatninger.

I PHP kan regex implementeres gjennom funksjoner som preg_match() og preg_replace(). Det er viktig å være forsiktig med å bruke regex, da det kan være ressurskrevende ved store tekstmengder og komplekse mønstre.

## Se også:
- [PHP.net - Regex](https://www.php.net/manual/en/book.pcre.php)
- [W3Schools - PHP Regex](https://www.w3schools.com/php/php_ref_regex.asp)
- [Regular-Expressions.info](https://www.regular-expressions.info/php.html)