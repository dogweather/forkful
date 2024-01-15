---
title:                "Extraction de sous-chaînes"
html_title:           "Python: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous cherchez à extraire des sous-chaines de caractères dans vos programmes Python ? Que ce soit pour le traitement de données, la manipulation de chaînes de caractères ou toute autre tâche, la manipulation de sous-chaines peut être une compétence utile pour tout Pythoniste.

## Comment faire

Le processus pour extraire des substrings en Python est relativement simple. Tout d'abord, vous devez définir la chaîne de caractères à partir de laquelle vous souhaitez extraire des sous-chaines. Ensuite, vous pouvez utiliser des indices ou des tranches pour spécifier les positions des caractères que vous souhaitez extraire.

Voici un exemple de code qui utilise une indexation et une tranche pour extraire des sous-chaines à partir d'une chaîne de caractères :

```
# Définir la chaîne de caractères
chaine = "Bonjour le monde !"

# Extraire une sous-chaine en utilisant un indice
sous_chaine_1 = chaine[8]
print(sous_chaine_1) # Résultat : l

# Extraire une sous-chaine en utilisant une tranche
sous_chaine_2 = chaine[3:7]
print(sous_chaine_2) # Résultat : jour
```

Vous pouvez également utiliser des méthodes de chaînes, telles que `split()` ou `replace()`, pour extraire des sous-chaines à partir de certains motifs ou motifs spécifiques.

## Plongée en profondeur

En Python, les chaînes de caractères sont considérées comme des "séquences", ce qui signifie qu'elles peuvent être traitées comme des listes ordonnées de caractères. Cela signifie que vous pouvez utiliser des boucles et des méthodes de listes, comme`append()` et`remove()`, pour modifier ou extraire des sous-chaines à partir de chaînes de caractères.

De plus, les chaînes de caractères en Python sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Lorsque vous utilisez des méthodes de chaînes pour extraire des sous-chaines, une nouvelle chaîne est en fait créée à partir de la chaîne d'origine. Cela peut affecter les performances si vous travaillez avec de grandes chaînes de caractères.

## Voir aussi

- [Documentation officielle de Python sur les chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)
- [Article sur GeeksforGeeks sur les méthodes de chaînes en Python](https://www.geeksforgeeks.org/python-string-methods-set-1/)
- [Tutoriel sur Real Python sur la manipulation de chaînes en Python](https://realpython.com/python-strings/)