---
title:    "Bash: Imprimer la sortie de débogage"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez sur un projet de programmation, il est parfois difficile de comprendre pourquoi votre code ne fonctionne pas comme prévu. C'est là qu'imprimer des messages de débogage peut être utile. Cela vous permet de visualiser les valeurs de variables et de vérifier si les conditions de vos boucles ou de vos instructions if sont remplies.

# Comment faire

Pour imprimer des messages de débogage en Bash, vous pouvez utiliser la commande "echo". Par exemple:

```Bash
x=5 # déclare une variable x et lui donne la valeur 5
echo "La valeur de x est $x" # imprime le message avec la valeur de la variable x
```

Cela vous permet de suivre l'exécution de votre code et de voir si les variables ont les valeurs attendues à chaque étape.

# Plongée en profondeur

Si vous souhaitez afficher des messages de débogage dans le terminal, vous pouvez utiliser la commande "printf". Voici un exemple:

```Bash
x=5
printf "La valeur de x est %d\n" $x
```

La différence avec "echo" est que vous avez plus de contrôle sur le formatage du message. En utilisant les spécificateurs de format, tels que "%d" pour une valeur numérique ou "%s" pour une chaîne de caractères, vous pouvez afficher les variables de manière précise.

Il est également possible de rediriger les messages de débogage vers un fichier en utilisant ">>". Cela peut être utile lorsque vous avez besoin d'enregistrer les messages pour une analyse ultérieure.

# Voir aussi

- [Documentation officielle sur la commande "echo"](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Documentation officielle sur la commande "printf"](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Tutoriel sur le débogage en Bash](https://devconnected.com/how-to-debug-bash-scripts/)