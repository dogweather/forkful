---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON, JavaScript Object Notation, är ett format för att lagra och utbyta data. Programmerare använder det för att smidigt skicka och ta emot data mellan servrar och webbklienter.

## Hur gör man:
Arbete med JSON i PHP är enkelt tack vare inbyggda funktioner som `json_encode()` och `json_decode()`. Här är snabbexempel.

```PHP
<?php
// Skapa en PHP-array
$data = array("namn" => "Anna", "ålder" => 25, "stad" => "Stockholm");

// Konvertera till JSON
$jsonData = json_encode($data);
echo $jsonData;
// Output: {"namn":"Anna","\u00e5lder":25,"stad":"Stockholm"}

// Konvertera tillbaka till PHP-objekt
$objektData = json_decode($jsonData);
print_r($objektData);
// Output: stdClass Object ( [namn] => Anna [ålder] => 25 [stad] => Stockholm )

// Konvertera tillbaka till PHP-array
$arrayData = json_decode($jsonData, true);
print_r($arrayData);
// Output: Array ( [namn] => Anna [ålder] => 25 [stad] => Stockholm )
?>
```

## Fördjupning
JSON introducerades 2001 och har sedan dess blivit webbens ledande dataformat. Alternativ inkluderar XML, men JSON vinner ofta på sin kompakthet. När du jobbar med PHP är komplicerade objekt och arrayer enkla att hantera som JSON, vilket är perfekt för API-kommunikation.

## Se också
- PHP manualen om JSON: https://www.php.net/manual/en/book.json.php
- JSON specifikation: https://www.json.org/json-en.html
- Jämförelse mellan JSON och XML: https://www.w3schools.com/js/js_json_xml.asp
