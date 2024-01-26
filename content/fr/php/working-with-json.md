---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec JSON en PHP, c'est gérer des données formatées, légères et universelles. Les programmeurs l'adorent pour sa simplicité et son intégrabilité aisée aux services web.

## How to:
### Encoder des données en JSON
```PHP
<?php
$data = ['nom' => 'Dupont', 'age' => 47];
$json = json_encode($data);
echo $json; // {"nom":"Dupont","age":47}
?>
```

### Décoder du JSON en PHP
```PHP
<?php
$json = '{"nom":"Dupont","age":47}';
$data = json_decode($json, true);
print_r($data); // Array ( [nom] => Dupont [age] => 47 )
?>
```

### Gestion des erreurs
```PHP
<?php
$json_errone = '{"nom":"Dupont",age:"47"}'; // Erreur de JSON
$data = json_decode($json_errone, true);
if (json_last_error() != JSON_ERROR_NONE) {
    echo json_last_error_msg(); // Syntax error
}
?>
```

## Deep Dive
JSON, "JavaScript Object Notation", a été inventé dans les années 2000, simplifiant la vie des programmeurs après XML. Alternatives incluent XML et YAML, mais la simplicité de JSON réside dans sa facilité à être analysée et générée par divers langages. En PHP, `json_encode()` et `json_decode()` sont les fonctions clés. Depuis PHP 5.2.0, le support JSON est implémenté nativement, et avec PHP 7, la performance s'est améliorée. Il est important de gérer les erreurs avec `json_last_error()` pour avoir des implémentations robustes.

## See Also
- Documentation PHP sur JSON: [PHP: JSON - Manual](https://www.php.net/manual/fr/book.json.php)
- JSONLint pour valider le JSON: [JSONLint](https://jsonlint.com/)
- JSON Formatter pour formater et colorer le JSON : [JSON Formatter & Validator](https://jsonformatter.curiousconcept.com/)
