---
title:                "Python: Utilisation des expressions régulières"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour les programmeurs Python. Elles permettent de rechercher et de manipuler du texte de manière plus efficace et précise. Si vous travaillez avec du texte dans vos projets, les expressions régulières peuvent grandement faciliter votre travail.

## Comment faire

Les expressions régulières sont disponibles en utilisant le module `re` en Python. Vous devez d'abord l'importer dans votre code:

```Python
import re
```

Pour utiliser les expressions régulières, vous devez d'abord définir un modèle à rechercher dans le texte. Par exemple, si vous voulez rechercher tous les mots commençant par "a", vous pouvez définir le modèle suivant:

```Python
pattern = '^a\w+'
```

Le symbole "^" signifie que la correspondance doit se trouver en début de ligne, suivi de la lettre "a" et du symbole "\w+" qui signifie un ou plusieurs caractères alphanumériques. Maintenant, vous pouvez rechercher ce modèle dans une chaîne de texte en utilisant la méthode `findall`:

```Python
text = "apple, banane, avion"
matches = re.findall(pattern, text)
print(matches)

# Output:
["apple", "avion"]
```

Vous pouvez également utiliser les expressions régulières pour remplacer du texte. Par exemple, si vous voulez remplacer tous les espaces par des tirets dans une chaîne, vous pouvez utiliser la méthode `sub`:

```Python
text = "Voici une phrase avec des espaces."
new_text = re.sub('\s', '-', text)
print(new_text)

# Output:
"Voici-une-phrase-avec-des-espaces."
```

## Plongée profonde

Les expressions régulières offrent une grande flexibilité dans la recherche et la manipulation de texte. Voici quelques éléments à retenir lorsque vous travaillez avec des expressions régulières en Python:

- Les symboles spéciaux tels que "^" et "\w" ont un sens précis dans les expressions régulières, donc assurez-vous de bien comprendre leur signification avant de les utiliser.
- Vous pouvez utiliser des groupes de correspondance pour sélectionner et manipuler des parties spécifiques du texte.
- Le module `re` propose de nombreuses fonctions utiles pour travailler avec les expressions régulières, comme `search` et `match`. N'hésitez pas à consulter la documentation officielle pour en savoir plus.

## Voir aussi

- [Documentation officielle Python sur les expressions régulières](https://docs.python.org/fr/3/library/re.html)
- [Cours sur les expressions régulières en français](https://openclassrooms.com/fr/courses/4425111-perfectionnez-vous-en-python/4463632-manipulez-les-expressions-regulieres)
- [Site permettant de tester des expressions régulières en ligne](https://regex101.com/)