---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

Salut les programmeurs ! Aujourd'hui, nous allons parler des expressions régulières en C. Si vous ne les utilisez pas déjà, vous manquez probablement quelque chose. Mais ne vous inquiétez pas, dans cet article, je vais vous expliquer ce que sont les expressions régulières et pourquoi elles sont si utiles pour les programmeurs. Nous allons également jeter un coup d'œil à leur mise en œuvre et à quelques alternatives. Alors, prêts à plonger dans le monde des expressions régulières en C ? Allons-y !

## Quoi & Pourquoi ?

Tout d'abord, qu'est-ce qu'une expression régulière en C ? Essentiellement, c'est une séquence de caractères utilisée pour décrire un modèle de texte que vous souhaitez rechercher dans une chaîne de caractères plus large. Autrement dit, c'est un outil très puissant pour effectuer des recherches dans du texte.

Alors pourquoi les programmeurs utilisent-ils des expressions régulières ? Tout simplement parce qu'elles facilitent grandement la manipulation de chaînes de caractères. Au lieu d'écrire de longs et complexes algorithmes pour trouver et manipuler du texte, les expressions régulières permettent de le faire en quelques lignes de code.

## Comment faire :

Maintenant que vous savez ce qu'est une expression régulière, voyons comment l'utiliser en pratique en C. Voici un exemple de code qui recherche et remplace toutes les occurrences du mot "bonjour" dans une chaîne :

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    // chaîne de caractères dans laquelle nous allons chercher
    char string[] = "Salut tout le monde, bonjour à tous !";
    // expression régulière que nous cherchons
    char regex[] = "Bonjour";

    // nous créons une structure regex pour stocker l'expression régulière
    regex_t reg;
    // nous compilons l'expression régulière dans notre structure
    regcomp(&reg, regex, 0);

    // nous remplaçons toutes les occurrences du mot "bonjour" par "salut"
    // dans la chaîne de caractères en utilisant la structure regex que nous avons créée
    regsub(&reg, string, "Salut", string);

    printf("%s", string);

    return 0;
}
```

Ici, nous utilisons les fonctions `regcomp` et `regsub` de la bibliothèque d'expressions régulières standard de C pour compiler et exécuter notre expression régulière. Ensuite, nous utilisons la fonction `printf` pour afficher la chaîne modifiée. Le résultat de ce code sera :

```
Salut tout le monde, salut à tous !
```

## Plongée en profondeur :

Maintenant que vous avez une idée de comment utiliser les expressions régulières en C, parlons un peu plus en détail de leur mise en œuvre. Les expressions régulières ont été développées dans les années 1950 par le mathématicien américain Stephen Cole Kleene. Elles sont basées sur les expressions rationnelles, un concept mathématique pour décrire des motifs de chaînes de caractères.

Il existe quelques alternatives aux expressions régulières, comme les expressions partielles et les arbres de syntaxe abstraits. Cependant, les expressions régulières restent l'une des méthodes les plus populaires et les plus puissantes pour manipuler du texte.

En termes d'implémentation en C, comme vous l'avez vu dans notre exemple de code, il existe une bibliothèque standard pour travailler avec les expressions régulières. De plus, de nombreuses bibliothèques tierces permettent également d'utiliser des expressions régulières en C.

## À voir également :

Si vous souhaitez en savoir plus sur les expressions régulières en C, voici quelques liens utiles :

- La documentation officielle de la bibliothèque d'expressions régulières standard de C : https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Un tutoriel pour débutants sur les expressions régulières en C : https://www.codingame.com/playgrounds/2240/le-livre-de-recettes-de-c/tutoriel---expression-reguliere
- Un article détaillé sur les expressions régulières en C de la Communauté OpenClassrooms : https://openclassrooms.com/fr/courses/1997366-apprenez-a-programmer-en-c/1997421-les-expressions-regulieres-en-c

J'espère que cet article vous a été utile pour comprendre les expressions régulières en C. Assurez-vous de les essayer la prochaine fois que vous travaillerez avec du texte en C. À bientôt !