---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON to format danych, wygodny do wymiany między językami programowania. Programiści używają go, bo jest lekki, czytelny dla człowieka i łatwy w parsowaniu.

## Jak to zrobić?
```php
<?php
// Przygotujmy tablicę do konwersji.
$dane = [
    'imie' => 'Jan',
    'nazwisko' => 'Kowalski',
    'wiek' => 30
];

// Konwertujemy tablicę do JSON.
$json = json_encode($dane);
echo $json;
// Wyjście: {"imie":"Jan","nazwisko":"Kowalski","wiek":30}

// Teraz zamienimy JSON z powrotem na tablicę PHP.
$daneZJson = json_decode($json, true);
print_r($daneZJson);
// Wyjście:
// Array
// (
//    [imie] => Jan
//    [nazwisko] => Kowalski
//    [wiek] => 30
// )
?>
```

## W głębi tematu
JSON, czyli JavaScript Object Notation, narodził się jako alternatywa dla XML. Jego format jest prostszy od XML i szybszy w przetwarzaniu. W PHP mamy wbudowaną obsługę JSON od wersji 5.2.0. Alternatywami dla JSON mogą być XML, YAML czy BSON, ale żaden nie dorównuje mu prostotą. 

Podczas pracy z JSON, należy pamiętać o obsłudze błędów. Funkcje `json_encode()` i `json_decode()` mogą zwrócić `null` przy napotkaniu błędu, co może być mylące, ponieważ `null` jest też poprawną wartością JSON. Użycie `json_last_error()` pomoże ustalić przyczynę błędu.

## Zobacz również
- Oficjalna specyfikacja JSON: http://json.org/
- Dokumentacja PHP na temat JSON: https://www.php.net/manual/pl/book.json.php
- Porównanie formatów danych JSON vs. XML: https://www.w3schools.com/js/js_json_xml.asp
