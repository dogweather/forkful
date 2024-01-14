---
title:    "Fish Shell: Utiliser des expressions régulières"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil essentiel pour les programmeurs et les développeurs qui souhaitent faire des recherches et des manipulations de données précises dans du texte. Elles permettent de rechercher des modèles de caractères spécifiques et de les utiliser pour filtrer et extraire des informations à partir de chaînes de caractères. Utiliser les expressions régulières dans le Shell Fish peut faciliter grandement le processus de traitement de données et rendre les codes plus efficaces.

## Comment faire

Vous pouvez utiliser les expressions régulières dans Fish Shell en utilisant la commande `grep`. Cette commande prend en paramètre une expression régulière et un fichier, puis renvoie les lignes correspondant au modèle de l'expression régulière dans le fichier. Voici un exemple d'utilisation :

```
Fish Shell> grep 'chat' mon_fichier.txt
Ce fichier contient des chats et des chiens.
```

Pour rechercher toutes les occurrences d'un modèle dans un fichier, ajoutez le drapeau `-i` pour ignorer la casse. Et si vous souhaitez uniquement extraire les lignes qui ne correspondent pas au modèle, utilisez le drapeau `-v`.

```
Fish Shell> grep -i 'chat' mon_fichier.txt
Ce fichier contient des chats et des chiens.
Il y a trois chAts sur mon canapé.
Il y a un caniche dans la cour.
```

```
Fish Shell> grep -v 'chat' mon_fichier.txt
Il y a un caniche dans la cour.
```

Vous pouvez également utiliser les expressions régulières dans les commandes de remplacement du Shell Fish, telles que `sed` et `awk`, pour manipuler davantage les données.

## Plongée en profondeur

Les expressions régulières peuvent sembler très intimidantes au début, mais une fois que vous comprendrez les concepts de base, vous pourrez en faire des utilisations puissantes et efficaces. Voici quelques astuces pour maîtriser les expressions régulières :

- Utilisez les classes de caractères pour regrouper les caractères similaires. Par exemple, `[aeiou]` correspondra à n'importe quelle voyelle, `[0-9]` correspondra à tout chiffre, et `[a-zA-Z]` correspondra à toute lettre majuscule ou minuscule.
- Utilisez les quantifieurs pour spécifier le nombre de fois qu'un modèle doit être recherché. Par exemple, `?` correspond à 0 ou 1 répétition, `*` correspond à 0 ou plus, et `+` correspond à 1 ou plus.
- Utilisez les expressions régulières non-greedy pour éviter que la recherche ne se propage à travers des parties inutiles du texte. Par exemple, en utilisant `.*?` à la place de `.*`, la recherche s'arrêtera à la première correspondance plutôt que d'aller jusqu'à la fin du texte.

## Voir aussi

- [La documentation de Fish Shell](https://fishshell.com/docs/current/cmds/grep.html)
- [Un tutoriel complet sur les expressions régulières](https://regexone.com/)
- [Un outil en ligne pour tester et affiner vos expressions régulières](https://regexr.com/)