---
title:                "Bash: Affichage du débogage informatique"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Bash, vous avez probablement déjà utilisé la commande "printf" pour afficher des informations à l'écran lors de l'exécution de votre script. Mais saviez-vous qu'il existe une autre méthode plus puissante pour afficher des informations de débogage dans votre code ? Dans cet article, nous allons plonger dans le monde de l'impression de sortie de débogage en Bash et découvrir pourquoi cela peut être utile pour améliorer la qualité de vos scripts.

## Comment faire

Pour afficher des informations de débogage dans votre code Bash, vous pouvez utiliser la commande "echo". Par exemple, si vous voulez afficher une variable, vous pouvez utiliser la syntaxe suivante :

```Bash
var="mon texte à afficher"
echo $var
```

Ce simple exemple imprimera "mon texte à afficher" à l'écran. Mais que se passe-t-il si vous souhaitez afficher plusieurs variables ou des informations plus complexes ? C'est là qu'interviennent les "printf statements". Avec cette commande, vous pouvez spécifier le format de vos sorties et utiliser des arguments pour afficher des informations supplémentaires.

Prenons un exemple concret. Supposons que nous ayons deux variables "nom" et "âge" que nous souhaitons afficher. Voici comment nous pouvons le faire avec une commande "printf" :

```Bash
name="Jean"
age=25
printf "Bonjour, je m'appelle %s et j'ai %d ans." $name $age
```

Ce code produira la sortie suivante :

```Bash
Bonjour, je m'appelle Jean et j'ai 25 ans.
```

Comme vous pouvez le constater, en utilisant la commande "printf", nous pouvons contrôler le format de notre sortie et afficher des informations de manière plus élaborée. Vous pouvez même utiliser des spécificateurs de format pour afficher des informations dans différents types de données, tels que des nombres binaires ou hexadécimaux.

## Plongée en profondeur

Maintenant que vous avez une idée de base de l'utilisation de la commande "printf" pour l'impression de sortie de débogage, voyons quelques astuces supplémentaires pour rendre cette technique encore plus puissante.

Tout d'abord, vous pouvez également utiliser la commande "printf" pour formater des chaines de caractères. Par exemple, si vous voulez ajouter des sauts de ligne dans votre sortie de débogage, vous pouvez utiliser le spécificateur de format "\n". De même, vous pouvez utiliser des tabulations avec le spécificateur de format "\t".

Deuxièmement, vous pouvez également utiliser la commande "printf" pour afficher des messages d'erreur. Pour ce faire, vous devez utiliser la redirection standard d'erreurs ">" pour rediriger la sortie vers la sortie standard des erreurs "2&1". Voici un exemple :

```Bash
printf "Il y a eu une erreur lors de l'exécution de cette commande." 2>&1
```

Enfin, il est également bon de savoir que vous pouvez utiliser des variables avec la commande "printf" pour rendre votre code plus dynamique. Vous pouvez utiliser des variables pour spécifier le format ou les arguments à utiliser. Par exemple :

```Bash
format="Le résultat de la commande %s est : %d"
cmd="ls -l"
printf "$format" $cmd $(eval $cmd)
```

Ici, nous avons spécifié le format et la commande à utiliser comme variables, ce qui nous permet de les changer facilement si nécessaire.

## Voir aussi

Pour en savoir plus sur l'impression de sortie de débogage en Bash, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de la commande "printf" en Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [Un tutoriel sur les spécificateurs de format en Bash](https://www.tutorialkart.com/bash-shell-scripting/bash-printf-format-examples/)
- [Des astuces pour utiliser efficacement la commande "printf" en Bash](https://wiki.bash-hackers.org/commands/builtin/printf)

Maintenant que vous avez une meilleure compréhension de l'impression de sortie de débogage en Bash, n'hésitez pas à l'utiliser dans vos scripts pour améliorer la qualité de votre code !