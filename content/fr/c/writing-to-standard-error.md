---
title:    "C: Écrire vers l'erreur standard"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C, vous avez probablement déjà rencontré l'utilisation de la fonction `fprintf()` pour afficher des messages d'erreur. Cependant, il existe une autre façon d'afficher des erreurs, en utilisant la sortie standard d'erreur, également connue sous le nom de "standard error". Dans cet article, nous allons expliquer pourquoi et comment vous devriez écrire vers la sortie standard d'erreur dans vos programmes C.

## Comment faire

Pour écrire vers la sortie standard d'erreur, vous devez utiliser la fonction `fprintf()` en remplaçant le premier argument par `stderr`. Par exemple, voici comment écrire un message d'erreur utilisant cette méthode :

```C
fprintf(stderr, "Erreur : impossible d'ouvrir le fichier.\n");
```

Ensuite, il vous suffit d'utiliser la fonction `perror()` pour afficher une description de l'erreur en plus du message que vous avez écrit :

```C
perror("Erreur");
```

Voici un exemple de sortie lorsque vous essayez d'ouvrir un fichier inexistant :

```
Erreur : impossible d'ouvrir le fichier.
Erreur : Aucun fichier ou dossier de ce type.
```

## Plongée en profondeur

Maintenant que vous savez comment écrire vers la sortie standard d'erreur, vous vous demandez peut-être pourquoi c'est une chose importante à faire. Il y a plusieurs raisons pour lesquelles il est recommandé d'utiliser la sortie standard d'erreur :

- Séparation du flux de sortie : la sortie standard d'erreur est spécifiquement destinée aux messages d'erreur, ce qui vous permet de séparer les messages d'erreur des autres messages que vous pouvez afficher dans votre programme.
- Affichage en temps réel : contrairement à la fonction `fprintf()`, qui stocke le message dans une mémoire tampon avant de l'afficher, la sortie standard d'erreur affiche immédiatement le message. Cela peut être utile lorsque vous rencontrez une erreur critique et que vous voulez en être informé immédiatement.
- Facilité de débogage : en utilisant la sortie standard d'erreur, vous pouvez facilement rediriger l'ensemble de vos messages d'erreur vers un fichier et l'utiliser pour déboguer votre programme.

## Voir aussi

- [La documentation officielle de la fonction fprintf](https://en.cppreference.com/w/c/io/fprintf)
- [Un tutoriel sur la sortie standard en C](https://www.cs.swarthmore.edu/~newhall/unixhelp/C_printf.html)
- [Un article sur la différence entre la sortie standard et la sortie standard d'erreur](https://www.explainshell.com/explain?cmd=2%3E%261)

---
Mettez à jour votre code C en utilisant la sortie standard d'erreur et améliorez votre débogage !