---
title:                "C: Écrire vers l'erreur standard"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture sur la sortie standard d'erreur, également appelée stderr, peut sembler être une tâche anodine pour les programmeurs débutants. Cependant, c'est une compétence essentielle pour déboguer et optimiser vos programmes. Dans cet article, nous allons vous expliquer pourquoi vous devriez apprendre à écrire sur stderr et comment le faire efficacement.

## Comment faire

Voici un exemple simple de code pour illustrer comment écrire sur stderr en utilisant la fonction `fprintf` :

```C
#include <stdio.h>

int main() {
    int num = 42;
    fprintf(stderr, "La réponse à tout est : %d\n", num);
    return 0;
}
```

La sortie standard d'erreur est généralement utilisée pour afficher des messages d'erreur ou de débogage pendant l'exécution du programme. En utilisant `fprintf` plutôt que `printf`, ces messages seront affichés sur stderr plutôt que sur la sortie standard (stdout). Cela permet de les différencier plus facilement et de les rediriger vers un fichier ou un autre processus si nécessaire.

Voici un exemple de sortie pour le code ci-dessus :

```text
La réponse à tout est : 42
```

Il est également possible d'utiliser la macro prédéfinie `stderr` plutôt que d'utiliser `fprintf` avec `stderr` en argument :

```C
#include <stdio.h>

int main() {
    int num = 42;
    fprintf(stderr, "La réponse à tout est : %d\n", num);
    return 0;
}
```

La syntaxe reste la même, mais l'utilisation de `stderr` peut rendre votre code plus lisible et plus facile à maintenir.

## Plongée en profondeur

L'écriture sur stderr peut sembler simple et indifférente, mais il y a quelques subtilités à prendre en compte. Tout d'abord, il est important de noter que la sortie standard d'erreur peut varier d'un système à l'autre, il est donc recommandé d'utiliser `stderr` plutôt que d'essayer de deviner le nom de fichier associé à la sortie standard d'erreur.

De plus, il est important de toujours vérifier si les appels à `fprintf` ont réussi en vérifiant la valeur de retour de la fonction. Si elle renvoie une valeur négative, cela indique qu'il y a eu une erreur d'écriture sur stderr.

Enfin, il est possible de rediriger la sortie standard d'erreur vers un fichier en utilisant la commande `2>` dans un terminal ou en utilisant la fonction `freopen` dans votre code. Cela peut être utile pour enregistrer les messages d'erreur dans un fichier de log pendant l'exécution du programme.

## Voir aussi

- [Documentation sur stderr en C](https://www.gnu.org/software/libc/manual/html_node/Output-to-Streams.html#index-stderr-1322)
- [Guide de débogage en C](http://www.cab.u-szeged.hu/local/teaching/csample/ch04s02.html)
- [Tutoriel sur la redirection de flux en C](https://www.tutorialspoint.com/c_standard_library/c_function_freopen.htm)