---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.077059-07:00
description: "JSON, ou JavaScript Object Notation, est un format l\xE9ger d'\xE9change\
  \ de donn\xE9es qui est facile \xE0 lire et \xE0 \xE9crire pour les humains, et\
  \ facile \xE0 analyser et\u2026"
lastmod: 2024-02-19 22:05:16.636336
model: gpt-4-0125-preview
summary: "JSON, ou JavaScript Object Notation, est un format l\xE9ger d'\xE9change\
  \ de donn\xE9es qui est facile \xE0 lire et \xE0 \xE9crire pour les humains, et\
  \ facile \xE0 analyser et\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
JSON, ou JavaScript Object Notation, est un format léger d'échange de données qui est facile à lire et à écrire pour les humains, et facile à analyser et à générer pour les machines. Les programmeurs travaillent souvent avec JSON pour échanger des données entre les serveurs et les applications web en raison de sa simplicité et de son indépendance linguistique, ce qui en fait une pierre angulaire dans le développement web moderne et les API.

## Comment faire :
Travailler avec JSON en PHP est simple grâce aux fonctions intégrées `json_encode()` et `json_decode()`. Ci-dessous se trouvent des exemples montrant comment convertir un tableau PHP en chaîne JSON, et inversement :

### Encodage d'un tableau PHP en chaîne JSON
```php
// Définir un tableau associatif
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Convertir le tableau PHP en chaîne JSON
$jsonString = json_encode($data);

// Afficher la chaîne JSON
echo $jsonString;
```
**Exemple de sortie :**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### Décodage d'une chaîne JSON en tableau PHP
```php
// Chaîne JSON
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// Convertir la chaîne JSON en tableau PHP
$data = json_decode($jsonString, true);

// Afficher le tableau PHP
print_r($data);
```
**Exemple de sortie :**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### Travailler avec une bibliothèque tierce : GuzzleHttp
Pour une gestion complexe des requêtes web et des données JSON, une bibliothèque PHP populaire est GuzzleHttp. Elle simplifie les requêtes HTTP et fonctionne facilement avec les données JSON.

**Installation via Composer :**
```
composer require guzzlehttp/guzzle
```

**Exemple de requête :**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Envoyer une requête à une API qui retourne du JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Décoder la réponse JSON en tableau PHP
$data = json_decode($response->getBody(), true);

// Afficher les données
print_r($data);
```

**En supposant que l'API renvoie des données JSON similaires :**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
Cela illustre la facilité d'utilisation de PHP pour la manipulation de JSON, à la fois avec les fonctions natives et avec des bibliothèques robustes comme GuzzleHttp pour des tâches plus complexes.
