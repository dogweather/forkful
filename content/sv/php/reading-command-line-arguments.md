---
title:    "PHP: Läsning av kommandoradsargument"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument kan vara användbart för att skapa mer dynamiska och flexibla PHP-program. Genom att läsa in argument från kommandoraden kan du få din kod att anpassa sig beroende på användarens inmatning, vilket kan göra dina program mer användarvänliga.

## Hur man gör

För att läsa in kommandoradsargument i PHP, används funktionen `getopt()`. Detta gör det möjligt att läsa in både korta och långa argument och till och med specificera vilka argument som krävs eller är valfria.

### Exempel

```PHP
// Skapa en array med argument som behövs eller kan användas
$argument = array(
    'f:' => 'first_name:',
    'l:' => 'last_name:',
    'u' => 'username'
);

// Använd getopt() för att läsa in kommandoradsargumenten
$options = getopt(null, $argument);

// Skriv ut resultatet
print_r($options);
```

### Output

- Om du kör scriptet med `php script.php -f John -l Doe` kommer resultatet att bli:

```
Array
(
    [first_name] => John
    [last_name] => Doe
)
```

- Om du kör scriptet med `php script.php -u -f John -l Doe` kommer resultatet att bli:

```
Array
(
    [username] =>
    [first_name] => John
    [last_name] => Doe
)
```

### Valida argument

För att specificera vilka argument som är obligatoriska eller valfria, använd `:` eller `::` efter argumentet.

- En dubbelkolon `::` betyder att argumentet är obligatoriskt och måste ha ett värde.
- En kolon `:` betyder att argumentet är valfritt men om det används måste det ha ett värde.

Det är också möjligt att specificera default-värden för valfria argument.

```PHP
$argument = array(
    'f:' => 'first_name:',
    'l::' => 'last_name::Test',
);
```

Dessa argument skulle läsa in `first_name` som obligatoriskt och `last_name` som valfritt med default-värdet "Test".

## Djupdykning

För mer avancerade användningsområden av kommandoradsargument läs dokumentationen för `getopt()`. Det finns också möjlighet att använda en annan funktion, `getarg()`, för att läsa in argument från HTML-formulär istället för kommandoraden.

## Se även

- Dokumentation för `getopt()`: [https://www.php.net/manual/en/function.getopt.php](https://www.php.net/manual/en/function.getopt.php)
- Dokumentation för `getarg()`: [https://www.php.net/manual/en/function.getarg.php](https://www.php.net/manual/en/function.getarg.php)