---
title:                "C: Utiliser les expressions régulières"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Les expressions régulières sont des outils puissants en programmation qui permettent de rechercher et manipuler des motifs de caractères dans une chaîne de texte. Elles sont largement utilisées pour valider des données, extraire des informations spécifiques et effectuer des opérations de recherche et de remplacement. Si vous souhaitez améliorer vos compétences en programmation et augmenter votre efficacité lors de la manipulation de chaînes de caractères, les expressions régulières sont un outil essentiel à maîtriser.

## Comment faire
Il existe de nombreuses bibliothèques de gestion d'expressions régulières en C, telles que PCRE (Perl Compatible Regular Expressions) et POSIX regex. Dans cet article, nous allons explorer l'utilisation de la bibliothèque POSIX pour démontrer comment utiliser les expressions régulières en C.
 
Les expressions régulières sont basées sur des motifs de caractères spécifiques, qui peuvent inclure des caractères littéraux, des métacaractères et des groupes de capture. Voici un exemple simple de code en C utilisant la bibliothèque POSIX qui recherche un motif de trois chiffres dans une chaîne de caractères :

```C
#include <stdio.h>
#include <regex.h> // bibliothèque POSIX

int main()
{
    regex_t regex; // structure pour stocker le motif
    char* string = "12345"; // chaîne de caractères à rechercher
    char* pattern = "[0-9]{3}"; // motif à rechercher (3 chiffres)

    regcomp(&regex, pattern, 0); // compilation du motif
    int result = regexec(&regex, string, 0, NULL, 0); // recherche du motif dans la chaîne de caractères

    if (result == 0) // si le motif est trouvé, retourne 0
    {
        printf("Motif trouvé !");
    }
    else // sinon, retourne une erreur
    {
        printf("Motif non trouvé...");
    }

    regfree(&regex); // libération de la mémoire utilisée par la structure regex

    return 0;
}
```
 
Dans cet exemple, nous utilisons la fonction `regcomp()` pour compiler le motif à rechercher et la fonction `regexec()` pour effectuer la recherche dans la chaîne de caractères. La fonction `regfree()` est ensuite utilisée pour libérer la mémoire allouée à notre structure `regex`. L'utilisation de ces fonctions peut sembler intimidante au début, mais en comprenant les notions de base des expressions régulières et en pratiquant, vous serez en mesure de les utiliser efficacement dans vos programmes en C.

## Plongée en profondeur
Les expressions régulières peuvent sembler un peu déroutantes au premier abord, mais une fois que vous en comprendrez les bases, elles deviendront un outil précieux pour manipuler les chaînes de caractères en C. En plus de la recherche et du remplacement de motifs, les expressions régulières peuvent également être utilisées pour valider des adresses email, des numéros de téléphone et d'autres formats de données couramment utilisés.

Il est important de noter que chaque bibliothèque de gestion d'expressions régulières peut avoir ses propres syntaxes et fonctionnalités, donc il est essentiel de vérifier la documentation de la bibliothèque que vous utilisez pour vous assurer de bien comprendre son fonctionnement.

## Voir aussi
- [Documentation officielle de la bibliothèque POSIX pour les expressions régulières](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/regex.h.html)
- [Tutorial sur les expressions régulières en C](https://www.tutorialspoint.com/regular-expression-with-using-posix-library-in-c-language)
- [Exemples pratiques d'utilisation des expressions régulières en C](https://www.informit.com/articles/article.aspx?p=1193856)