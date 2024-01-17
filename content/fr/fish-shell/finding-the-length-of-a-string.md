---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Fish Shell: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#Qu'est-ce que c'est et pourquoi le faire ?

Trouver la longueur d'une chaîne de caractères est simplement le fait de compter le nombre de caractères présents dans cette chaîne. Les programmeurs le font souvent pour vérifier la validité des données entrées par l'utilisateur ou pour effectuer des opérations sur des chaînes de caractères.

#Comment faire :

```Fish Shell
# Pour trouver la longueur d'une chaîne de caractères, utilisez la fonction "string length"
set chaine "Bonjour!"
set longueur (string length $chaine)
echo "La chaîne '$chaine' contient $longueur caractères."
```

Résultat attendu :
```
La chaîne 'Bonjour!' contient 8 caractères.
```

#Plongée en profondeur :

Historiquement, la recherche de la longueur d'une chaîne de caractères était souvent réalisée en bouclant à travers chaque caractère de la chaîne et en comptant manuellement. Cependant, avec l'évolution des langages de programmation, des fonctions prédéfinies telles que "string length" sont disponibles pour simplifier cette tâche.
Dans d'autres langages de programmation, vous pouvez également utiliser la méthode "length" pour trouver la longueur d'une chaîne de caractères.

#Voir aussi :

- Documentation officielle sur la fonction "string length" de Fish Shell : https://fishshell.com/docs/current/cmds/string-length.html
- Documentation officielle sur la méthode "length" en JavaScript : https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length