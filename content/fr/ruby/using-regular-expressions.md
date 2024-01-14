---
title:    "Ruby: Utiliser des expressions régulières"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà ressenti une certaine frustration lors de la recherche de données spécifiques dans un texte? Heureusement, avec Ruby, il y a une solution: les expressions régulières! Les expressions régulières sont des séquences de caractères utilisées pour trouver des motifs spécifiques dans une chaîne de texte. Cela peut sembler intimidant pour certains, mais une fois que vous aurez maîtrisé les expresssions régulières, vous pourrez effectuer des tâches telles que la validation de données, le nettoyage de texte et la recherche de motifs spécifiques dans des chaînes de texte.

## Comment faire

Pour utiliser des expressions régulières en Ruby, vous devez d'abord déclarer votre motif en utilisant des slashes, comme ceci: ```Ruby /motif/ ``` Ensuite, vous pouvez utiliser plusieurs méthodes pour comparer votre motif à une chaîne de texte. Par exemple, vous pouvez utiliser la méthode match, qui renvoie un objet Match si le motif est trouvé dans la chaîne de texte, sinon il renvoie nil. Exemple:

```Ruby
/texte = "Je suis un programmeur passionné de Ruby!"
texte.match(/Ruby/) # renvoie #<MatchData "Ruby">
```

Vous pouvez également utiliser la méthode scan pour renvoyer un tableau contenant toutes les correspondances trouvées, ou la méthode sub pour remplacer le motif par une autre valeur. Voici un exemple d'utilisation de ces méthodes:

```Ruby
/texte = "J'aime apprendre Ruby, mais je n'aime pas la syntaxe compliquée."
texte.scan(/ruby|syntaxe/) #renvoie ["Ruby", "syntaxe"]
texte.sub(/ruby/, "coding") #renvoie "J'aime apprendre coding, mais je n'aime pas la syntaxe compliquée."
```

## Plongée en profondeur

Les expressions régulières peuvent sembler un peu déroutantes au début, mais il y a plusieurs ressources disponibles pour vous aider à les comprendre davantage. Une bonne façon de se familiariser avec les expressions régulières en Ruby est de les pratiquer sur des plateformes d'exercices en ligne telles que HackerRank ou Codewars. De plus, la documentation officielle de Ruby fournit une liste complète des méthodes et opérateurs d'expressions régulières disponibles en Ruby. N'hésitez pas à l'utiliser comme référence lors de l'écriture de vos propres expressions régulières.

## Voir aussi

- [Documentation officielle de Ruby sur les expressions régulières](https://ruby-doc.org/core/Regexp.html)
- [Exercices d'expression régulières sur HackerRank](https://www.hackerrank.com/domains/regex)
- [Codewars: apprenez les expressions régulières en pratiquant](https://www.codewars.com/kata/search/train/ruby?q=regexp)