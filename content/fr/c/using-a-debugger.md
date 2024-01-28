---
title:                "Utilisation d'un débogueur"
date:                  2024-01-26T03:47:52.752100-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un débogueur est un outil qui vous permet d'inspecter votre code C pendant qu'il s'exécute, étape par étape, pour traquer les bugs. Les programmeurs utilisent des débogueurs pour comprendre le comportement de leur code, corriger les problèmes et optimiser les performances sans jouer à un jeu de devinettes.

## Comment faire :
Disons que vous travaillez avec un programme C simple qui calcule le factoriel d'un nombre, mais il y a un glitch. Pour utiliser un débogueur comme `gdb` (GNU Debugger), compilez d'abord avec l'option `-g` pour inclure les informations de débogage :

```c
// compiler avec : gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Un simple contrôle pour l'entrée négative
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Le factoriel de %d est %ld\n", number, result);
    return 0;
}
```

Ensuite, exécutez-le dans gdb :

```shell
$ gdb ./factorial
```

Placez un point d'arrêt sur la fonction `factorial` et lancez le programme :

```gdb
(gdb) break factorial
(gdb) run
```

Quand il atteint le point d'arrêt, parcourez chaque ligne en utilisant `next` ou `n` et inspectez les variables avec `print` ou `p` :

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

La sortie d'exemple fournira des valeurs en temps réel et le flux d'exécution du programme.

## Plongée Profonde
Les débogueurs existent depuis les années 1960, évoluant de simples moniteurs vers des applications complexes basées sur des interfaces graphiques. Le débogage basé sur l'impression était courant avant le développement de débogueurs matures. Parmi les alternatives à `gdb`, on trouve `lldb`, `dbx` ou les débogueurs intégrés dans les IDE comme ceux de Visual Studio ou CLion.

Lorsqu'il s'agit de débogueurs, l'implémentation varie : certains peuvent détecter les erreurs d'exécution, examiner la mémoire, ou même inverser l'exécution d'un programme. `gdb` peut se connecter à des processus en cours d'exécution, permettant le débogage de logiciels déjà en fonctionnement, un avantage pour la correction des bugs des systèmes en direct.

## Voir Aussi
- Débogueur GNU (GDB) : https://www.gnu.org/software/gdb/documentation/
- Débogage avec GDB : https://sourceware.org/gdb/current/onlinedocs/gdb
- Débogueur LLDB : https://lldb.llvm.org/use/tutorial.html
- Techniques de débogage en C : http://www.cprogramming.com/debugging/debugging.html
