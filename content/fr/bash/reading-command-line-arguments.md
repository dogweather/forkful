---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Lire les arguments de la ligne de commande, c'est récupérer les données passées à votre script Bash lors de son exécution. Les programmateurs le font pour rendre leurs scripts plus flexibles et réutilisables.

## Comment Faire : 

Voici comment lire les arguments de la ligne de commande en Bash :

```Bash
#!/bin/bash 
   
echo "Le premier argument est : $1"
echo "Le deuxième argument est : $2"
echo "Le troisième argument est : $3"
```
Exemple de sortie si vous exécutez le script avec `./myscript.sh arg1 arg2 arg3`
```
Le premier argument est : arg1
Le deuxième argument est : arg2
Le troisième argument est : arg3
```

## Plongée en Profondeur :

(1) Contexte historique : Bash, introduit dans les années 80, prend en charge le passage d'arguments sur la ligne de commande depuis ses débuts.

(2) Alternatives: D'autres langages de script comme Python et Ruby ont leurs propres méthodes pour lire les arguments de la ligne de commande.

(3) Détails de mise en œuvre : En Bash, les variables spéciales (`$1`, `$2`, `$3`, etc.) sont utilisées pour lire les arguments. `$0` représente le nom du script lui-même. Vous pouvez aussi utiliser `$#` pour obtenir le nombre d'arguments.

## Voir Aussi :

- [Guide Bash Avancé](http://tldp.org/LDP/abs/html/)
- [Guide Bash pour les Débutants](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Argument Parsing en Bash](https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/)