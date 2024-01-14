---
title:    "Javascript: Utiliser les expressions régulières"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Pourquoi

Les expressions régulières, également connues sous le nom de "regex", sont un outil puissant pour manipuler des chaînes de caractères dans les programmes JavaScript. Elles permettent de trouver et de remplacer des motifs spécifiques dans une grande quantité de données, ce qui simplifie grandement le traitement et la manipulation de ces données. Si vous travaillez avec des chaînes de caractères dans vos programmes JavaScript, les expressions régulières sont un incontournable à apprendre.

##Comment faire

Les expressions régulières sont créées en utilisant la notation littérale /regex/, où "regex" est le motif que vous souhaitez rechercher. Par exemple, si vous voulez trouver tous les mots dans une phrase qui commencent par la lettre "a", vous pourriez utiliser la regex /a\w*/. 

```
// Exemple de regex pour trouver les mots commençant par "a"
let phrase = "Un ami aime les ananas";
let regex = /a\w*/g;
let resultats = phrase.match(regex);

console.log(resultats);
// Output : ["ami", "ananas"]
```

Dans cet exemple, la regex /a\w*/ correspond à tous les mots commençant par la lettre "a", suivis de n'importe quel nombre de lettres ou de chiffres (le symbole * indique un nombre quelconque de caractères). L'ajout du modificateur g (pour "global") permet de trouver tous les mots correspondant à ce modèle dans la phrase, et la méthode `match()` renvoie un tableau contenant les résultats.

Les expressions régulières offrent également de nombreuses options pour effectuer des recherches plus complexes, telles que la recherche de motifs précis, l'utilisation de caractères spéciaux et la gestion de métacaractères. Il est important de pratiquer et d'expérimenter avec différents modèles et modificateurs pour vraiment maîtriser l'utilisation des regex.

##Plongée en profondeur

Pour aller plus loin dans l'utilisation des expressions régulières, voici quelques points à retenir :

- Les caractères spéciaux tels que \d pour les chiffres ou \s pour les espaces peuvent être utilisés pour rechercher des motifs spécifiques dans une chaîne de caractères.
- Les métacaractères comme ^ pour indiquer le début d'une ligne et $ pour indiquer la fin d'une ligne peuvent ajouter de la précision à vos recherches.
- Les modificateurs tels que i (pour "insensible à la casse") ou m (pour "recherche mutliligne") peuvent être utilisés pour affiner vos recherches.
- Les groupes de capture et les références arrières peuvent être utilisés pour extraire des parties spécifiques d'une chaîne de caractères correspondant à un motif.

Il existe de nombreuses ressources en ligne pour apprendre en profondeur les expressions régulières, ainsi que des sites de test pour expérimenter avec différents modèles et entrées de données.

##Voir aussi

- [Documentation de Mozilla sur les expressions régulières en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/RegExp)
- [Exercices de regex interactifs sur RegexOne](https://regexone.com/)
- [Site de test de regex en ligne Regex101](https://regex101.com/)