---
title:    "Python: Utiliser les expressions régulières"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant et essentiel pour tout développeur Python. Elles permettent de rechercher et de manipuler rapidement et efficacement du texte en utilisant des modèles de caractères spécifiques. Les développeurs utilisent souvent les expressions régulières pour traiter des données, valider des entrées utilisateur ou filtrer des chaînes de caractères. Cet article expliquera comment utiliser les expressions régulières en Python et pourquoi elles sont si utiles.

## Comment faire

Tout d'abord, il est nécessaire d'importer le module ```re``` en utilisant la commande suivante :

```
import re
```

Ensuite, vous pouvez utiliser la fonction ```re.search()``` pour rechercher un modèle spécifique dans une chaîne de caractères. Par exemple, si vous voulez trouver une adresse e-mail dans une chaîne de caractères, vous pouvez utiliser le code suivant :

```
email = "contact@exemple.com"
pattern = r"[a-z0-9]+@[a-z]+\.[a-z]+"
match = re.search(pattern, email)
if match:
    print("Adresse e-mail trouvée !")
else:
    print("Aucune adresse e-mail trouvée.")
```

Ce code recherchera une adresse e-mail valide dans la variable ```email``` en utilisant un modèle d'expression régulière. Le résultat sera affiché en fonction de la présence ou l'absence d'une correspondance.

Vous pouvez également utiliser la fonction ```re.sub()``` pour remplacer une partie d'une chaîne de caractères en utilisant un modèle d'expression régulière. Par exemple, si vous voulez remplacer toutes les occurrences de lettres majuscules dans une chaîne de caractères par des lettres minuscules, vous pouvez utiliser le code suivant :

```
string = "Bonjour LES AMIS !"
pattern = r"[A-Z]"
new_string = re.sub(pattern, "a-z", string)
print(new_string) # Affichera "Bonjour les amis !"
```

## Plongée en profondeur

Les expressions régulières offrent une grande flexibilité et de nombreuses options pour rechercher et manipuler du texte en Python. Vous pouvez utiliser des caractères spéciaux tels que ```*``` pour représenter zéro ou plusieurs occurrences d'un caractère, ```+``` pour représenter une ou plusieurs occurrences et ```?``` pour représenter zéro ou une occurrence. Vous pouvez également utiliser des groupes de caractères tels que ```[a-z]``` pour représenter n'importe quelle lettre minuscule.

De plus, les expressions régulières en Python prennent en charge les expressions conditionnelles, les opérateurs de mise en correspondance et les modifieurs. Il y a beaucoup plus à apprendre sur les expressions régulières en Python, et il est fortement recommandé de se plonger dans leur documentation officielle pour en savoir plus.

## Voir aussi

- Documentation officielle des expressions régulières en Python : https://docs.python.org/fr/3/library/re.html
- Tutoriel sur les expressions régulières en Python : https://realpython.com/regex-python/
- Testeur d'expressions régulières en ligne : https://regex101.com/