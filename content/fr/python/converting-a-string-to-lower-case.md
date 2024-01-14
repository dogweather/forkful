---
title:    "Python: Convertissement d'une chaîne en minuscules"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de convertir des chaînes de caractères en lettres minuscules lors de la programmation en Python. Cela peut être utile pour des comparaisons de chaînes de caractères sans tenir compte de la casse ou pour améliorer la lisibilité du code.

## Comment faire

Pour convertir une chaîne de caractères en lettres minuscules en Python, nous pouvons utiliser la méthode `lower()`. Voici un exemple de code et sa sortie :

```Python
# Définir une chaîne de caractères
chaine = "PROGRAMMATION EN PYTHON"
# Convertir en lettres minuscules
chaine_min = chaine.lower()
# Afficher la chaîne convertie
print(chaine_min)
```

Sortie :

```Python
programmation en python
```

## Approfondissement

La méthode `lower()` est un exemple de méthode de manipulation de chaînes de caractères en Python. Elle ne modifie pas la chaîne originale, mais renvoie plutôt une nouvelle chaîne avec les caractères en lettres minuscules. Cette méthode peut également être utilisée pour des chaînes de caractères contenant des caractères spéciaux ou des chiffres.

Il est important de noter que la conversion en lettres minuscules dépend de la langue et du jeu de caractères utilisés. Pour les langues avec des caractères accentués, la méthode `lower()` peut ne pas modifier les lettres spéciales. Il est donc recommandé d'utiliser la fonction `unidecode` du module `python-unicodedata2` pour une conversion plus complète.

## Voir aussi

- Documentation officielle Python : [Manipulation de chaînes de caractères](https://docs.python.org/fr/3/tutorial/introduction.html#strings)
- Documentation du module `python-unicodedata2` : [unidecode](https://pypi.org/project/python-unicodedata2/)
- Tutoriel en français sur la manipulation de chaînes de caractères en Python : [Les chaînes de caractères en Python](https://openclassrooms.com/fr/courses/235344-apprenez-a-programmer-en-python/232916-le-code-des-caracteres)