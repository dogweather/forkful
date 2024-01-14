---
title:                "Arduino: Utilisation des expressions régulières"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser des expressions régulières dans la programmation Arduino ?

Les expressions régulières sont des outils puissants pour traiter et manipuler des chaînes de caractères dans la programmation Arduino. Elles permettent de rechercher des motifs spécifiques dans les données et de les manipuler de manière précise et efficace. Elles peuvent être utilisées dans de nombreux projets, que ce soit pour valider des saisies utilisateur, extraire des informations à partir de données ou encore vérifier le format de messages reçus.

## Comment utiliser les expressions régulières dans votre code Arduino ?

Pour utiliser les expressions régulières dans votre code Arduino, vous devez tout d'abord inclure la bibliothèque "Regex.h". Ensuite, vous pouvez utiliser la fonction "match()" pour rechercher un motif spécifique dans une chaîne de caractères. Par exemple, si vous souhaitez vérifier si une adresse email est valide, vous pouvez utiliser la syntaxe suivante :

```Arduino
#include <regex.h>

regex email_pattern("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}");

// Vérifie si l'adresse email saisie correspond au motif
if (regex_match(email, email_pattern)) {
  // Code à exécuter si l'email est valide
} else {
  // Code à exécuter si l'email est invalide
}
```

Vous pouvez également utiliser des expressions régulières pour extraire des informations d'une chaîne de caractères à l'aide de la fonction "smatch". Par exemple, si vous avez une chaîne de caractères contenant une date au format "jour/mois/année", vous pouvez extraire chaque élément en utilisant la syntaxe suivante :

```Arduino
#include <regex.h>

// Chaîne contenant la date au format "jour/mois/année"
string date = "31/03/2020";

// Motif pour extraire chaque élément de la date
regex date_pattern("([0-9]{2})/([0-9]{2})/([0-9]{4})");

smatch date_results;

// Utilise la fonction regex_search pour rechercher le motif dans la chaîne
regex_search(date, date_results, date_pattern);

// Stocke chaque élément dans une variable séparée
string day = date_results[1];
string month = date_results[2];
string year = date_results[3];
```

## Plongée en profondeur dans l'utilisation des expressions régulières

Les expressions régulières peuvent être utilisées avec une grande variété de caractères spéciaux et de combinaisons pour rechercher des motifs encore plus précis. Vous pouvez également utiliser des fonctions de remplacement pour modifier les chaînes de caractères correspondantes. Il existe de nombreux tutoriels et ressources en ligne pour approfondir vos connaissances sur les expressions régulières, ainsi que des sites pour tester vos expressions en ligne. N'hésitez pas à explorer et à expérimenter pour améliorer vos compétences en utilisant ces puissants outils de programmation.

## Voir aussi les liens suivants pour en savoir plus sur les expressions régulières :

- [Tutoriel sur les expressions régulières par W3Schools](https://www.w3schools.com/python/python_regex.asp)
- [Référence de la bibliothèque regex pour Arduino](https://github.com/madmann91/Regex)
- [Tester des expressions régulières en ligne avec Regex101](https://regex101.com/)
- [Utiliser des expressions régulières dans la programmation Arduino par Chris Ruppel](https://chrizzbee.blog/how-to-use-regular-expressions-in-arduino-programming/)