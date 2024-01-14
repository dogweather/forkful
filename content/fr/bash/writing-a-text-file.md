---
title:    "Bash: Écrire un fichier texte"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en programmation Bash ?

L'écriture d'un fichier texte est une tâche courante dans la programmation Bash. Cela peut être nécessaire pour enregistrer des données, des résultats ou des paramètres pour une utilisation ultérieure. Écrire un fichier texte peut également être utile pour générer des rapports ou pour créer une configuration personnalisée pour un script.

## Comment procéder ?

Pour écrire un fichier texte en Bash, il suffit d'utiliser la commande `echo` suivie du texte à écrire, entre guillemets, et de rediriger la sortie vers un fichier en utilisant l'opérateur `>`.

Exemple:

```Bash
echo "Bonjour, voici mon texte." > mon_fichier.txt
```

Le contenu entre guillemets sera écrit dans le fichier `mon_fichier.txt`.

Pour ajouter du texte à un fichier existant, on peut utiliser l'opérateur `>>` qui ajoute le texte après le contenu déjà existant sans l'écraser.

Exemple:

```Bash
echo "Nouvelle ligne" >> mon_fichier.txt
```

Le texte "Nouvelle ligne" sera ajouté à la fin du fichier `mon_fichier.txt`.

## Plongeon en profondeur

Il existe d'autres commandes utiles pour écrire dans un fichier texte en Bash, telles que `printf` qui permet un contrôle plus précis sur le formatage du texte à écrire, ou `cat` qui peut être utilisée pour combiner plusieurs fichiers en un seul. La redirection de la sortie avec l'opérateur `>` peut également être remplacée par la commande `tee`, qui permet d'écrire à la fois sur la sortie standard et dans un fichier.

De plus, il est important de noter que l'écriture de fichiers textes en Bash peut être combinée avec d'autres concepts tels que les boucles ou les commandes conditionnelles pour une utilisation plus avancée.

## Voir aussi

- [Bash Redirection Documentation](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections)
- [Bash printf Command Help](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#printf-Builtin)
- [Bash cat Command Help](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#The-cat-Builtin)
- [Bash tee Command Help](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#The-tee-Builtin)