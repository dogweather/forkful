---
title:                "Travailler avec json"
html_title:           "PHP: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-json.md"
---

{{< edit_this_page >}}

Qu'est-ce que JSON et pourquoi est-ce que les programmeurs l'utilisent ?

JSON (JavaScript Object Notation) est un format de données basé sur le JavaScript qui permet aux programmeurs de stocker et de transférer des données sous forme de texte. Cela en fait un choix populaire pour l'échange de données sur le web car il est facile à lire et à écrire pour à la fois les humains et les machines. Les programmeurs utilisent JSON pour stocker des données structurées telles que des listes, des tableaux et des objets, en les rendant faciles à manipuler et à utiliser dans leurs applications.

Comment faire ?

Voici un exemple de code PHP pour encoder des données en JSON :

```
$objet = [
  "nom" => "John Doe",
  "age" => 30,
  "hobbies" => ["lecture", "voyage", "musique"]
];
$encodage = json_encode($objet);
echo $encodage;
```
Résultat :

```{"nom":"John Doe","age":30,"hobbies":["lecture","voyage","musique"]}```

Pour décoder des données JSON dans un tableau PHP :

```
$json = '{"nom":"John Doe","age":30,"hobbies":["lecture","voyage","musique"]}';
$tableau = json_decode($json, true);
print_r($tableau);
```
Résultat :

```Array
(
    [nom] => John Doe
    [age] => 30
    [hobbies] => Array
        (
            [0] => lecture
            [1] => voyage
            [2] => musique
        )
)```

Plongeon en profondeur

JSON a été créé par Douglas Crockford en 2001 et est devenu un format de données populaire dans les applications web depuis lors. Il est très similaire à la notation d'objets de JavaScript, ce qui facilite son utilisation pour les développeurs web. Alternativement, les programmeurs peuvent utiliser d'autres formats de données tels que XML ou CSV, mais ils peuvent être plus complexes à lire et à écrire pour les humains.

JSON peut également être utilisé avec d'autres langages de programmation en plus de PHP, en faisant un choix polyvalent pour échanger des données entre différentes plates-formes.

Voir aussi

Pour en savoir plus sur JSON et son utilisation avec PHP, consultez les ressources suivantes :

- La documentation officielle de PHP sur la manipulation de données JSON : https://www.php.net/manual/fr/ref.json.php
- Le site officiel de JSON : https://www.json.org/
- Un tutoriel sur l'utilisation de JSON dans les applications web : https://openclassrooms.com/fr/courses/2091901-prise-en-main-de-json/2092231-fonctionnalites