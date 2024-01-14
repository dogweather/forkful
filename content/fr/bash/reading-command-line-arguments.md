---
title:    "Bash: Lecture des arguments de la ligne de commande"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Bonjour chers lecteurs ! Aujourd'hui, nous allons explorer une fonctionnalité essentielle lors de la programmation en Bash : la lecture des arguments en ligne de commande. Si vous êtes un débutant en Bash ou si vous souhaitez rafraîchir vos connaissances sur le sujet, cet article est fait pour vous !

## Pourquoi

Avant de plonger dans les détails techniques, il est important de comprendre pourquoi il est important de connaître la lecture des arguments en ligne de commande en Bash. Tout d'abord, il s'agit d'une compétence fondamentale pour tout développeur en Bash. En utilisant cette fonctionnalité, vous pourrez rendre vos scripts plus flexibles et personnalisables en fonction des différents paramètres fournis par l'utilisateur au moment de l'exécution. De plus, cela vous permettra également d'automatiser davantage vos tâches et de gagner du temps lors de l'exécution de vos scripts.

Maintenant que vous comprenez l'importance de la lecture des arguments en ligne de commande, passons à la partie pratique.

## Comment faire

Pour lire les arguments en ligne de commande en Bash, nous utiliserons la variable d'environnement `$@` qui contient tous les arguments fournis par l'utilisateur au moment de l'exécution du script. Voici un exemple de code :

```Bash
#!/bin/bash

# stockage des arguments dans une variable
arguments=$@

# affichage du nombre d'arguments fournis
echo "Nombre d'arguments : $#"

# affichage de chaque argument
for argument in $arguments; do
  echo $argument
done
```

Voici un exemple d'exécution et de sortie :

```Bash
$ ./read_args.sh foo bar baz
Nombre d'arguments : 3
foo
bar
baz
```

Comme vous pouvez le voir, nous avons stocké les arguments dans une variable `$arguments` et nous avons utilisé un compteur `$#` pour afficher le nombre total d'arguments fournis. Ensuite, nous avons utilisé une boucle for pour parcourir chaque argument et l'afficher individuellement.

Il est également important de noter que vous pouvez également utiliser des options courtes (`-`) et longues (`--`) lors de la lecture des arguments en ligne de commande en utilisant la commande `getopts`. Cela peut être utile pour rendre vos scripts plus conviviaux pour les utilisateurs.

## Approfondissement

Maintenant que vous avez compris les bases de la lecture des arguments en ligne de commande en Bash, voici quelques conseils supplémentaires pour vous aider à aller plus loin :

- Utilisez la commande `shift` pour supprimer le premier argument de la variable `$@` après l'avoir utilisé. Cela permettra de traiter les arguments un par un plutôt que de tous les traiter en une seule fois.
- Vous pouvez également utiliser la commande `getopts` avec des drapeaux pour rendre votre script plus interactif et demander à l'utilisateur des informations supplémentaires en fonction des options choisies.
- Prenez en compte la validation des arguments, c'est-à-dire vérifier si les arguments fournis sont valides avant de les utiliser dans votre script.

Maintenant que vous avez une meilleure compréhension de la lecture des arguments en ligne de commande en Bash, vous pouvez l'appliquer à vos propres scripts et les rendre plus flexibles et puissants !

## Voir aussi

Pour en savoir plus sur la lecture des arguments en ligne de commande en Bash, voici quelques ressources utiles :

- Le guide officiel de Bash sur les arguments en ligne de commande : https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables
- Un tutoriel vidéo de Codecademy sur les arguments en ligne de commande en Bash : https://www.youtube.com/watch?v=DamzjNwM-jI
- Une liste de bonnes pratiques pour la lecture des arguments en ligne de commande en Bash : https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash/13359121#13359121

Ceci conclut notre exploration de la lecture des arguments en ligne de commande en Bash. J'espère que cet article vous a été utile et que vous serez maintenant en mesure d'utiliser cette fonctionnalité dans vos propres scripts Bash. Bonne programmation !