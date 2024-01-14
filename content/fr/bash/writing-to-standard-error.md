---
title:                "Bash: Écriture vers l'erreur standard"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire sur l'erreur standard en Bash

Si vous êtes programmeur en Bash, vous avez probablement déjà entendu parler de l'erreur standard ou "standard error". Mais pourquoi est-il important d'écrire sur l'erreur standard ? En bref, cela permet de mieux gérer les erreurs et les messages d'erreur lors de l'exécution d'un script. Sans écrire correctement sur l'erreur standard, il peut être difficile de savoir où se produit une erreur et comment la résoudre.

# Comment écrire sur l'erreur standard en Bash

La syntaxe pour écrire sur l'erreur standard est assez simple. Tout ce dont vous avez besoin est la commande "echo" suivie du message d'erreur entre guillemets. Par exemple, si vous voulez afficher le message "Erreur : fichier introuvable", vous utiliserez la commande suivante :

```Bash
echo "Erreur : fichier introuvable"
```

Le résultat sera affiché dans le terminal de la manière suivante :

```
Erreur : fichier introuvable
```

Il est également possible d'afficher des messages d'erreur plus détaillés en utilisant les variables spéciales "stderr" et ">&2". Ces variables envoient le message directement à l'erreur standard plutôt qu'à la sortie standard.

```Bash
echo "Une erreur s'est produite" 1>&2
echo "Code d'erreur : 404" >&2
```

Le résultat sera ainsi affiché :

```
Une erreur s'est produite
Code d'erreur : 404
```

# Plongée profonde dans l'écriture sur l'erreur standard

L'erreur standard est en fait un flux de données qui permet d'afficher des messages d'erreur ou de débogage dans le terminal. Cela permet de séparer les messages d'erreur des messages de sortie standard, qui sont généralement affichés à l'utilisateur.

En utilisant la syntaxe ">&2", vous pouvez rediriger les messages d'erreur vers l'erreur standard et ainsi les afficher en temps réel. Cela est particulièrement utile lorsque vous exécutez un script et que vous voulez voir les messages d'erreur en même temps que les messages de sortie standard.

# Voir aussi
- [Guide de référence Bash](https://www.gnu.org/software/bash/manual/)
- [Comment utiliser les variables spéciales en Bash](https://www.linuxjournal.com/content/return-values-bash-scripts)
- [Comprendre l'écriture sur l'erreur standard en Bash](https://www.baeldung.com/linux/bash-redirect-error-output)