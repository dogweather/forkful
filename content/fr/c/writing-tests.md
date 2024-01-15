---
title:                "Écrire des tests"
html_title:           "C: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en C

Si vous développez des applications en C, il est nécessaire de tester votre code pour assurer sa fiabilité et sa qualité. Les tests vous permettent de détecter des bugs, de valider des fonctionnalités et d'améliorer la maintenance de votre code.

## Comment écrire des tests en C

Pour écrire des tests en C, vous avez besoin de la bibliothèque de test standard de C, "assert.h". Elle fournit des macros pour effectuer différentes vérifications sur des expressions et des variables. Voici un exemple de code avec une assertion basique :

```C
#include <assert.h>

int somme(int a, int b) {
    return a + b;
}

int main() {
    int resultat = somme(2, 2);
    assert(resultat == 4);
    return 0;
}
```
Lorsque vous exécutez ce code, si la condition n'est pas satisfaite, le programme s'arrêtera et affichera un message d'erreur contenant l'expression qui a échoué et le numéro de ligne. Essayez de changer le résultat de l'addition pour voir comment le test échoue.

## Plongée profonde

Il existe d'autres fonctionnalités dans la bibliothèque "assert.h" qui permettent de tester plus en détail votre code. Vous pouvez notamment utiliser la macro "assert_true" pour tester si une expression est vraie ou "assert_false" pour tester si elle est fausse. De plus, en utilisant la macro "assert_fail", vous pouvez définir vous-même un message d'erreur plus précis en cas d'échec du test.

N'oubliez pas que les tests doivent être écrits à l'avance et ne doivent pas dépendre d'une logique métier complexe, car ils sont conçus pour tester des comportements précis et spécifiques de votre code.

# Voir aussi

- [Documentation de la bibliothèque assert.h](https://www.gnu.org/software/libc/manual/html_node/Assert.html)
- [Article sur les bonnes pratiques de test en C](https://hackaday.com/2017/02/07/embed-with-elliot-better-firmware-through-testing/)
- [Exemple de tests unitaires en C avec la bibliothèque Unity](https://www.throwtheswitch.org/unity)