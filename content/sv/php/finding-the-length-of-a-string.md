---
title:                "PHP: Att hitta längden på en sträng."
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift inom programmering och är användbart i många olika situationer. Det kan hjälpa dig att kontrollera inmatade data, formatera utdata och utföra olika manipulationer på strängar.

## Hur man gör det
För att hitta längden på en sträng i PHP, används den inbyggda funktionen `strlen()`. Den här funktionen tar emot en sträng som argument och returnerar dess längd som ett heltal.

```PHP
<?php

$str = "Hej, världen!";
echo strlen($str); // Output: 13

?>
```

I exemplet ovan skapar vi en variabel `$str` som innehåller strängen "Hej, världen!". Därefter använder vi funktionen `strlen()` för att räkna ut längden på strängen och skriver ut resultatet med hjälp av `echo`. I detta fall blir resultatet 13 eftersom strängen består av 13 tecken.

Du kan också använda `strlen()` för att kontrollera om en sträng är tom eller inte. Om funktionen returnerar värdet 0 betyder det att strängen är tom.

```PHP
<?php

$str = "";
if (strlen($str) === 0) {
  echo "Strängen är tom.";
} else {
  echo "Strängen har en längd på " . strlen($str) . " tecken.";
}

?>
```

I detta exempel kontrollerar vi längden på strängen och skriver ut ett meddelande baserat på resultatet. Om längden är 0, skriver vi ut att strängen är tom, annars skriver vi ut längden på strängen.

## Deep Dive
I PHP betraktas en sträng som en array av tecken, vilket betyder att du kan använda `strlen()` för att räkna ut längden på varje enskilt tecken. Detta kan vara användbart om du behöver utföra mer avancerade manipulationer på en sträng.

```PHP
<?php

$str = "Hej, världen!";
for ($i = 0; $i < strlen($str); $i++) {
  echo $str[$i] . " "; // Output: H e j ,  v ä r l d e n !
}

?>
```

I exemplet ovan använder vi en `for`-loop för att loopa igenom varje tecken i strängen och skriva ut dem med ett mellanrum emellan.

Om du behöver utföra manipulationer på en sträng baserat på dess längd, kan du också använda `strlen()` i kombination med andra inbyggda funktioner som `substr()` och `str_replace()`. Det finns många olika tillämpningar av att hitta längden på en sträng, så det är definitivt en grundläggande kunskap att ha i sin verktygslåda som PHP-utvecklare.

## Se även
- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [PHP strlen() function](https://www.php.net/manual/en/function.strlen.php)
- [PHP Substr() function](https://www.php.net/manual/en/function.substr.php)
- [PHP str_replace() function](https://www.php.net/manual/en/function.str-replace.php)