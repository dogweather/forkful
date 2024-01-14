---
title:                "Bash: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou un développeur expérimenté, vous avez peut-être entendu parler de JSON. Il s'agit d'un format de données populaire utilisé pour stocker et partager des informations. Mais pourquoi serait-il important de travailler avec JSON dans vos projets de programmation ?

En bref, JSON est un format léger, facile à lire et à écrire, et surtout compatible avec de nombreux langages de programmation. Cela en fait un choix idéal pour échanger des données entre différentes applications ou services. De plus, avec la popularité croissante de l'utilisation d'APIs, la manipulation de JSON devient de plus en plus nécessaire pour les développeurs.

## Comment faire

Maintenant que vous savez pourquoi JSON est important, passons à la partie pratique. Heureusement, travailler avec JSON en Bash est très facile grâce à une commande intégrée : `jq`.

```Bash
# Exemple de code pour extraire une valeur spécifique d'un fichier JSON
mon_dossier_json='{"nom":"Jean-Luc","age":30,"ville":"Paris"}'
echo "$mon_dossier_json" | jq '.ville'
```

L'output de ce code sera `Paris`, ce qui montre comment `jq` peut facilement extraire une valeur en utilisant une expression régulière pour la cible. Vous pouvez également utiliser `jq` pour traiter des fichiers JSON plus complexes et les convertir en tableaux ou en objets.

```Bash
# Exemple de code pour convertir un fichier JSON en un tableau de valeurs
mon_autre_dossier_json='{"noms":["Sophie","Pierre","Marie"],"ages":[25,34,29]}'
echo "$mon_autre_dossier_json" | jq '.ages[]'
```

L'output de ce code sera une liste des âges : `25 34 29`. Comme vous pouvez le voir, la manipulation de JSON en Bash est simple et pratique grâce à `jq`.

## Plongée en profondeur

Bien sûr, il est important de comprendre les bases de la manipulation de JSON en Bash, mais cela peut également être utile de connaître quelques astuces avancées pour les projets plus complexes. Tout d'abord, `jq` permet d'utiliser des opérateurs logiques pour filtrer les données, ce qui peut être très pratique lorsque vous travaillez avec de grandes quantités de données.

De plus, en utilisant la commande `curl`, vous pouvez facilement récupérer des données JSON depuis des APIs externes et les manipuler en utilisant `jq` pour extraire les informations spécifiques qui vous intéressent.

## Voir aussi

Maintenant que vous en savez plus sur la manipulation de JSON en Bash, il pourrait être utile de jeter un coup d'œil à ces ressources pour approfondir vos connaissances :

- Tutoriel sur la manipulation de JSON en Bash : https://www.tutorialspoint.com/bash_parsing_json.htm
- Documentation officielle de `jq` : https://stedolan.github.io/jq/
- Exemples de projets utilisant `jq` pour la manipulation de JSON : https://github.com/fiatjaf/awesome-jq

Vous êtes maintenant prêt à intégrer la manipulation de JSON dans vos projets Bash. N'hésitez pas à expérimenter avec `jq` et à découvrir toutes ses possibilités pour tirer le meilleur parti de ce format de données.