---
title:    "Fish Shell: Utiliser les expressions régulières"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur expérimenté ou novice dans le domaine de la programmation, vous avez probablement rencontré des situations où vous avez besoin de rechercher, filtrer ou remplacer des chaînes de caractères dans un fichier ou un texte. C'est là que les expressions régulières entrent en jeu. Les expressions régulières, également appelées regex, sont un ensemble de règles utilisées pour rechercher et manipuler des chaînes de caractères selon un modèle spécifique. En utilisant des expressions régulières, vous pouvez économiser du temps et des efforts en évitant de devoir écrire du code personnalisé pour chaque cas.

## Comment faire 

Voici un exemple de code en utilisant les expressions régulières dans Fish Shell pour rechercher et afficher tous les mots commençant par la lettre "a" dans un fichier texte:
```Fish Shell
grep -oE "\ba\w+" fichier.txt
```

Dans ce code, nous utilisons la commande "grep" pour rechercher le modèle spécifié par les expressions régulières. L'option "-o" permet d'afficher seulement les parties correspondantes, et l'option "-E" est utilisée pour activer les expressions régulières. Le modèle "\ba\w+" recherche tous les mots commençant par la lettre "a" suivie de n'importe quel caractère alphanumérique. En utilisant "\b" au début du modèle, nous nous assurons que le mot commence par la lettre "a" et non une partie d'un mot. Enfin, nous spécifions le nom du fichier dans lequel nous voulons effectuer la recherche.

Voici un exemple de sortie possible pour ce code:
``` 
apple
appleauce
apricot
avocado
```

## Plongée en profondeur 

Les expressions régulières peuvent sembler intimidantes au début, car elles utilisent des caractères spéciaux et des symboles pour définir des modèles. Voici quelques-uns des éléments clés à connaître pour mieux comprendre les expressions régulières:

- Les métacaractères: ces caractères spéciaux ont une signification spécifique dans les expressions régulières. Par exemple, "." correspond à n'importe quel caractère.
- Les quantificateurs: ils spécifient le nombre de fois qu'un élément doit apparaître dans une chaîne de caractères. Par exemple, "a{2,5}" signifie que l'on recherche une chaîne contenant entre 2 et 5 fois la lettre "a".
- Les caractères de classe: ils permettent de rechercher un certain type de caractère. Par exemple, "\d" correspond à un chiffre et "\w" correspond à un caractère alphanumérique.

Pour en savoir plus sur les expressions régulières et leurs nombreuses utilisations, n'hésitez pas à consulter les ressources ci-dessous.

## Voir aussi 

- [La documentation officielle de Fish Shell](http://fishshell.com/docs/current/)
- [Les bases des expressions régulières en Fish Shell](https://swcarpentry.github.io/shell-novice/05-regex/index.html)
- [RegexOne - Un tutoriel interactif pour apprendre les expressions régulières](https://regexone.com)