---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

# PHP String Interpolation: En effektiv lösning för att hantera strängar i PHP

## Vad och Varför?
String interpolation är tekniken att infoga variabler direkt i en sträng. Det gör koden renare, lättare att förstå och mindre benägen för fel.

## Så här gör du:
För att interpolera en sträng i PHP, använd "raka citattecken" och placera variabeln inne i citattecknen.

```PHP
$name = "Sofia";
echo "Hej, $name!"; // Skriver ut: Hej, Sofia!
```
Observera skillnaden jämfört med enkel citationsteknik:

```PHP
echo 'Hej, $name!'; // Skriver ut: Hej, $name!
```
I det första exemplet tolkas `$name` som en variabel och ersätts med dess värde. I det andra exemplet tolkas `$name` bokstavligen som en textsträng.


## Djupdykning:
### Historisk Kontext:
Stringinterpolation är ingen ny teknik. Det har använts i programmering sedan tidiga dagar av kommandoskaltolkar som Unix shell, Perl, och det är tillgängligt i många moderna språk som Ruby, JavaScript (ES6) och naturligtvis PHP.

### Alternativ:
Ett alternativ till stränginterpolation är att använda konkatenering, men detta gör koden svårare att läsa och skriva, särskilt för stora strängar med många variabler. 

```PHP
echo 'Hej, ' . $name . '!';
```

Kom ihåg, att i PHP fungerar stränginterpolation bara med dubbelsidiga citattecken. 

### Implementation detaljer
Under huven konverterar PHP en interpolerad sträng till en konkatenerad sträng innan den körs. Så `"Hej, $name!"` blir i själva verket `'Hej, ' . $name . '!'`. 

## Se Även:
För mer information om stränginterpolation och strängar i PHP, se följande källor:
1. [PHP Dokumentation - Strängar](https://www.php.net/manual/se/language.types.string.php)
2. [PHP: The Right Way - Strängar](https://phptherightway.com/#strings)
3. [PHP String Interpolation vs Concatenation](https://stackoverflow.com/questions/1837432/php-string-interpolation-vs-concatenation)