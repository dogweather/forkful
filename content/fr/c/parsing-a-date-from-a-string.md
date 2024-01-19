---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'analyse d'une date à partir d'une chaîne est le processus de conversion d'une date représentée sous forme de texte en format de date compréhensible par le programme. Les programmeurs le font pour manipuler et comparer les dates efficacement.

## Comment faire :
Voici un exemple simple qui montre comment analyser une date à partir d'une chaîne en utilisant la fonction `strptime` de la bibliothèque time.h.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char buf[255];

    const char *str = "06/04/2021";
    strptime(str, "%m/%d/%Y", &tm);
    
    strftime(buf, sizeof(buf), "%d %B, %Y", &tm);
    printf("Date formatée : %s\n", buf);

    return 0;
}
```
Le résultat de cet exemple sera :

```
Date formatée : 04 juin, 2021
```

## Plongeon Profond :
Historiquement, la conversion de chaînes de date était un processus délicat et sujet à des erreurs. C'est pourquoi la fonction `strptime` a été introduite dans la bibliothèque time.h pour faire cela de manière systématique.

Il existe d'autres alternatives pour analyser une date à partir d'une chaîne. Par exemple, vous pouvez utiliser des fonctions spécifiques à une date comme `sscanf` pour extraire et convertir les dates, mais cela peut être plus complexe et sujet à des erreurs.

L'implémentation détaillée du `strptime` peut varier selon les systèmes. Cependant, l'idée de base est de lire la chaîne de caractères en se basant sur le format de date et de remplir la structure `tm` avec des valeurs appropriées.

## Voir Aussi :
Pour plus d'informations sur la bibliothèque time.h et ses fonctions, vous pouvez consulter les liens suivants :
1. Manuel GNU C Library : https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html
2. Documentation du développeur Apple pour time.h : https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/time.3.html
3. Page du manuel Linux pour strptime: https://man7.org/linux/man-pages/man3/strptime.3.html
4. Sources complémentaires sur le C : https://www.learn-c.org/