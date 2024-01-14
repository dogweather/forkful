---
title:    "C: Commencer un nouveau projet"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles quelqu'un pourrait vouloir se lancer dans un nouveau projet de programmation en C. Peut-être qu'ils ont une idée de projet passionnante qu'ils veulent réaliser, ou qu'ils cherchent à améliorer leurs compétences en programmation. Quelle que soit la raison, c'est une excellente opportunité pour apprendre et s'exprimer à travers le langage de programmation C.

## Comment faire

Pour commencer un projet en C, vous aurez besoin d'un environnement de développement intégré (IDE) et d'un compilateur C. Vous pouvez utiliser l'IDE de votre choix, mais si vous êtes nouveau en programmation C, je recommande d'utiliser un IDE tel que Code::Blocks ou Eclipse pour faciliter la compilation et le débogage.

Une fois que vous avez votre IDE et votre compilateur en place, vous pouvez commencer par créer un nouveau fichier source C. Vous pouvez le nommer comme vous le souhaitez, mais assurez-vous de lui donner l'extension ".c". Ensuite, vous pouvez écrire votre code dans ce fichier en utilisant les structures de programmation de base telles que les boucles, les conditions et les fonctions.

Voici un exemple de code qui imprime les nombres pairs de 0 à 10 :

```C
#include <stdio.h>

int main()
{
    int i;

    for(i = 0; i <= 10; i++)
    {
        if(i % 2 == 0)
        {
            printf("%d ", i);
        }
    }

    return 0;
}
```

Lorsque vous exécutez ce code, vous devriez voir la sortie suivante :

```
0 2 4 6 8 10
```

Félicitations, vous avez écrit votre premier programme en C! N'hésitez pas à expérimenter avec différents codes pour en apprendre davantage sur le langage C.

## Plongée en profondeur

Comme mentionné précédemment, il est important de choisir un IDE et un compilateur appropriés pour votre projet. Cependant, il est également essentiel de comprendre les concepts de base de la programmation en C tels que les types de données, les opérateurs et les pointeurs.

Les types de données déterminent la taille et la valeur qu'un objet peut contenir. Les opérateurs sont utilisés pour effectuer des opérations sur ces données et les pointeurs sont des variables qui contiennent des adresses de mémoire. Ces concepts peuvent sembler complexes au début, mais en les pratiquant, vous serez en mesure de mieux les comprendre et de les utiliser dans vos projets.

De plus, il est important de bien organiser votre code en utilisant des commentaires et des conventions de codage telles que le style de nommage des variables. Cela rendra votre code plus facile à lire et à maintenir, en particulier lorsqu'il s'agit de projets plus importants.

## Voir aussi

- [Tutoriel de programmation en C pour débutants](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c)
- [Guide officiel de référence du langage C](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html)
- [Chaîne YouTube "TheNewBoston" pour des tutoriels C complets](https://www.youtube.com/playlist?list=PL2_aWCzGMAwLSqGsERZGXGkA5AfMhcknE)