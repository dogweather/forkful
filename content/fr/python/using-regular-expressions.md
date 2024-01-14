---
title:    "Python: Utilisation des expressions régulières"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont des outils très utiles en programmation pour manipuler et rechercher des motifs spécifiques dans une chaîne de caractères. Elles permettent de simplifier et d'accélérer le traitement des données en automatisant des tâches complexes. Utiliser des expressions régulières peut également améliorer la qualité et la fiabilité de votre code.

## Comment Faire

Pour utiliser des expressions régulières en Python, vous devez importer le module "re". Voici un exemple de code qui utilise une expression régulière pour trouver tous les nombres contenus dans une chaîne de caractères :

```Python
import re

chaine = "Il y a 12 chiens dans le parc."

resultats = re.findall(r'\d+', chaine) # recherche tous les nombres dans la chaîne

print(resultats)
```

Résultat :

```Python
['12']
```

La fonction "findall" recherche tous les motifs correspondants dans la chaîne et les renvoie sous forme d'une liste. Le "r" en début d'expression régulière indique que la chaîne est une chaîne brute, ce qui vous permet d'écrire des expressions sans avoir à échapper les caractères spéciaux.

## Plongée en Profondeur

Les expressions régulières peuvent être utilisées pour des tâches plus avancées telles que valider des adresses e-mail, extraire des informations spécifiques d'un texte ou même vérifier la syntaxe d'un code source. Elles sont également très utiles pour nettoyer les données avant de les utiliser dans des analyses ou des modèles de machine learning.

Un autre élément important à savoir est le concept de groupes et de correspondances dans les expressions régulières. Les parenthèses dans une expression régulière définissent un groupe, qui peut être utilisé pour extraire des parties spécifiques du texte. Par exemple, si votre chaîne contient des numéros de téléphone français au format "XX-XX-XX-XX-XX", vous pouvez utiliser l'expression régulière suivante pour extraire ces numéros :

```Python
resultats = re.findall(r'(\d{2}-){4}(\d{2})', chaine)
```

Ici, les groupes définis par les parenthèses sont utilisés pour correspondre au format spécifique des numéros de téléphone français et renvoyer uniquement les chiffres dans ces groupes.

## Voir Aussi

Pour plus d'informations sur l'utilisation des expressions régulières en Python, consultez les ressources suivantes :

- [Documentation officielle de Python sur les expressions régulières](https://docs.python.org/fr/3/library/re.html)
- [Tutoriel sur les expressions régulières en Python](https://openclassrooms.com/fr/courses/4425111-perfectionnez-vous-en-python/4934976-utilisez-des-expressions-regulieres)
- [Outil de test en ligne pour les expressions régulières en Python](https://regex101.com/)