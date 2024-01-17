---
title:                "Utiliser les expressions régulières"
html_title:           "Python: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Les expressions régulières sont un moyen puissant de traiter et de manipuler des chaînes de caractères dans un programme Python. Elles permettent de rechercher et de remplacer des motifs spécifiques dans du texte, ce qui les rend utiles pour réaliser des tâches telles que la validation de formulaires ou la recherche de mots-clés dans un document. De nombreux programmeurs utilisent les expressions régulières pour renforcer l'efficacité de leur code et automatiser certaines tâches récurrentes.

## Comment faire:
Voici un exemple de code Python utilisant des expressions régulières pour vérifier si une adresse email est valide:

```Python
import re

email = input("Saisissez votre adresse email: ")

if re.search(r"^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$", email):
    print("Votre adresse email est valide.")
else:
    print("Veuillez saisir une adresse email valide.")
```

**Sortie:**
```
Saisissez votre adresse email: example@domaine.com
Votre adresse email est valide.
```

## Plongée en profondeur:
Les expressions régulières ont été inventées dans les années 1950 par un mathématicien américain nommé Stephen Cole Kleene. Elles sont basées sur la théorie des automates et elles sont maintenant disponibles dans de nombreux langages de programmation, y compris Python. Bien que puissantes, les expressions régulières peuvent être difficiles à lire et à écrire, surtout pour les débutants. Heureusement, il existe des alternatives telles que les méthodes de manipulation de chaînes de caractères intégrées à Python.

## Voir aussi:
- [Documentation officielle de Python sur les expressions régulières](https://docs.python.org/fr/3/library/re.html)
- [Site de regex101 pour tester et apprendre les expressions régulières](https://regex101.com/)