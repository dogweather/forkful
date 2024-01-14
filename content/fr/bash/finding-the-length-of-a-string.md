---
title:    "Bash: Trouver la longueur d'une chaîne de caractères"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi
Trouver la longueur d'une chaîne de caractères est une tâche très courante en programmation. Cela peut être utile pour compter le nombre de lettres dans un mot ou pour vérifier si une chaîne dépasse une limite de longueur spécifique. Dans cet article, nous allons vous montrer comment trouver rapidement et facilement la longueur d'une chaîne en utilisant Bash.

## Comment faire
Tout d'abord, vous devez saisir la chaîne dont vous souhaitez connaître la longueur. Cela peut être une saisie utilisateur ou une variable définie dans le script. Pour cet exemple, nous allons utiliser une variable appelée "texte" avec la valeur "Bonjour tout le monde !" :

```Bash
texte="Bonjour tout le monde !"
```

Ensuite, nous utiliserons la commande "echo" pour afficher la chaîne et la fonction "wc" pour compter le nombre de caractères :

```Bash
echo $texte | wc -c
```

Le paramètre "-c" de la commande "wc" indique la comptabilisation des caractères. En exécutant ces commandes dans un terminal, vous obtiendrez une sortie de 22, ce qui correspond au nombre de caractères dans la chaîne "Bonjour tout le monde !".

Vous pouvez également utiliser la syntaxe suivante pour compter le nombre de caractères dans une chaîne sans utiliser la commande "echo" :

```Bash
wc -c <<< $texte
```

Cette syntaxe utilise le "here-string" (<<<) pour envoyer la chaîne directement à la commande "wc".

## Plongez plus profondément
En interne, la fonction "wc" utilise une boucle pour parcourir chaque caractère de la chaîne et l'incrémenter dans son compteur. Une autre manière de compter le nombre de caractères dans une chaîne avec Bash est d'utiliser une boucle "for" et la commande "expr". Considérez l'exemple suivant :

```Bash
for ((i=0; i<${#texte}; i++)); do
  compteur=$(expr $compteur + 1)
done
```

Ici, nous parcourons la chaîne caractère par caractère en utilisant la variable "i" comme index. À chaque itération, nous utilisons "expr" pour ajouter 1 au compteur. À la fin de la boucle, le compteur contiendra le nombre total de caractères dans la chaîne.

## Voir aussi
- [Documentation sur la commande wc](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)
- [Introduction à la programmation avec Bash](https://www.scripting-tutorial.com/fr/bash-scripting-tutorial/)