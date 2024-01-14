---
title:    "Bash: Affichage des sorties de débogage"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

L'impression des données de débogage peut sembler être une étape inutile ou fastidieuse dans la programmation en Bash. Cependant, cela peut s'avérer être un outil précieux pour comprendre le comportement de votre code et trouver rapidement des erreurs.

## Comment faire

La commande "echo" est l'un des moyens les plus simples d'imprimer des données de débogage dans Bash. Vous pouvez l'utiliser pour afficher des variables, des messages et même des résultats de commandes. Voici un exemple de code pour imprimer le contenu d'une variable :

```Bash
num=5
echo "Le nombre est $num"
```

Cela affichera le résultat suivant dans le terminal :
```
Le nombre est 5
```

Vous pouvez également utiliser la commande "printf" pour un contrôle plus précis de la mise en forme de votre sortie. Voici un exemple de code pour afficher le contenu de deux variables dans un message formaté :

```Bash
name="Marie"
age=25
printf "Bienvenue %s, vous avez %d ans." $name $age
```

Cela affichera le résultat suivant :
```
Bienvenue Marie, vous avez 25 ans.
```

## Profonde plongée

Il existe plusieurs façons d'optimiser l'impression des données de débogage dans Bash. Vous pouvez par exemple utiliser des niveaux de débogage pour contrôler quelle sortie est affichée en fonction de votre environnement de développement. Cela peut être utile si vous voulez éviter de surcharger la sortie pendant l'exécution du code en production.

Il est également possible d'écrire des fonctions spéciales pour gérer l'impression des données de débogage. Cela peut vous aider à garder votre code propre et à éviter la duplication de code pour l'impression de données similaires.

## Voir aussi

- [Guide complet sur l'utilisation de la commande "echo"](https://www.tutorialspoint.com/unix_commands/echo.htm)
- [Tutoriel "printf" en Bash](https://www.linux.com/blog/beginners-guide-formatting-output-bash)
- [Article sur l'optimisation de l'impression des données de débogage en Bash](https://www.shellhacks.com/quick-catch-to-adjust-bash-debug-output/)