---
title:                "Bash: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il y a souvent des moments où l'on souhaite comprendre en détail ce qui se passe dans notre code. Il peut être difficile de suivre toutes les étapes d'exécution, les valeurs des variables et les erreurs éventuelles. C'est là que l'impression de sorties de débogage entre en jeu. Cela nous permet de voir exactement ce qui se passe étape par étape, et nous permet de cibler les problèmes plus rapidement.

## Comment faire

Pour imprimer des sorties de débogage dans Bash, nous utilisons la commande `echo` suivie de la variable ou de la valeur que nous souhaitons imprimer. Cela peut être utile lors du débogage de fonctions pour vérifier les valeurs de retour ou lors de l'évaluation de conditions dans des scripts.

```
# Exemple de sortie de débogage
ma_variable="test"
echo "La valeur de ma_variable est : $ma_variable"
```

Cela produirait la sortie suivante :

```
La valeur de ma_variable est : test
```

On peut également utiliser l'option `-e` pour formater la sortie de manière plus lisible :

```
# Formatage de la sortie de débogage avec -e
ma_variable="test"
echo -e "La valeur de ma_variable est : \n$ma_variable"
```

Cela produirait la sortie suivante :

```
La valeur de ma_variable est :
test
```

Nous pouvons également imprimer des messages de débogage en utilisant la commande `printf`, qui permet un contrôle plus précis sur la mise en forme des sorties.

```
# Utilisation de printf pour imprimer des sorties de débogage
ma_variable="test"
printf "La valeur de ma_variable est : %s\n" $ma_variable
```

Cela produirait la même sortie que l'exemple précédent :

```
La valeur de ma_variable est : test
```

## Plongée en profondeur

Outre l'utilisation de base de la commande `echo` et `printf` pour imprimer des sorties de débogage, il existe également d'autres techniques avancées que nous pouvons utiliser. Une façon est d'utiliser le débogage intrinsèque de Bash en utilisant `set -x`. Cela activera le mode de débogage et affichera toutes les étapes d'exécution ainsi que les valeurs des variables. Pour désactiver le mode, nous utilisons `set +x`.

Nous pouvons également utiliser `trap` pour afficher des messages de débogage en temps réel lors de l'exécution d'un script. Il suffit de déclarer une fonction qui contient le message que nous souhaitons afficher et de l'associer à une action spécifique :

```
# Utilisation de trap pour afficher des messages de débogage en temps réel
#!/bin/bash

# Déclaration de la fonction de débogage
function afficher_debogage() {
  echo "Débogage : Petites étapes"
}

# Associer la fonction à une action spécifique avec trap
trap afficher_debogage DEBUG
```

Chaque fois que `DEBUG` est appelé, la fonction sera exécutée et le message de débogage sera affiché.

## Voir aussi

- [Guide de débogage Bash](https://wiki.bash-hackers.org/scripting/debuggingtips)
- [Blog sur l'utilisation de l'impression de débogage en Bash](https://www.shellscript.sh/debugging.html)
- [Documentation sur la commande `echo`](https://www.gnu.org/software/coreutils/manual/html_node/echo-invocation.html)
- [Documentation sur la commande `printf`](https://www.gnu.org/software/coreutils/manual/html_node/printf-invocation.html)