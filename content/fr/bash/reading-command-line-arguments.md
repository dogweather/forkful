---
title:    "Bash: Lecture des arguments de ligne de commande"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

La manipulation des arguments de ligne de commande est un aspect important de la programmation en Bash. Cela permet aux utilisateurs d'exécuter des commandes spécifiques en entrant des paramètres personnalisés. En apprenant à lire les arguments de ligne de commande, vous pourrez créer des scripts interactifs et rendre votre programme plus flexible pour les utilisateurs.

## Comment Faire

Pour lire les arguments de ligne de commande en Bash, vous devez utiliser la variable spéciale "$ @". Cette variable stocke tous les arguments saisis par l'utilisateur et vous pouvez y accéder en utilisant une boucle for. Voici un exemple:

```Bash
for arg in "$@" 
do
  echo "Argument: $arg"
done
```

Dans cet exemple, chaque argument sera imprimé sur une ligne séparée. Vous pouvez également utiliser la variable "$#" pour obtenir le nombre total d'arguments saisis par l'utilisateur.

## Plongée Profonde

Les arguments de ligne de commande peuvent également être définis comme des options en utilisant le caractère "-" suivi d'une lettre ou d'un mot. Par exemple, si vous utilisez "Bash script.sh -v", le script reconnaîtra l'option "-v" et l'exécutera en conséquence. Vous pouvez également utiliser la commande "getopts" pour traiter et récupérer ces options dans votre programme.

De plus, les arguments de ligne de commande peuvent également être utilisés pour spécifier des fichiers ou des répertoires à traiter par votre programme. Vous pouvez utiliser la commande "shift" pour déplacer les arguments et les options vers la gauche, ce qui vous permet de traiter un argument à la fois.

## Voir Aussi

- [Documentation officielle Bash sur les arguments de ligne de commande](https://www.gnu.org/software/bash/manual/html_node/Command-Line-Arguments.html)
- [Tutoriel sur les arguments de ligne de commande en Bash](https://www.tutorialkart.com/bash-shell-scripting/bash-arguments/)
- [Commandes utiles pour travailler avec les arguments de ligne de commande en Bash](https://bash.cyberciti.biz/guide/Accessing_command_line_parameters)