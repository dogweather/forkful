---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lire les arguments de la ligne de commande consiste à évaluer et utiliser les paramètres que vous entrez lorsque vous exécutez un programme en console. Les programmeurs utilisent cette technique pour personnaliser le comportement du programme sans avoir à modifier le code source.

## Comment faire:

Voici comment lire les arguments de la ligne de commande en C++. Commencez par comprendre que `main()` peut prendre deux arguments: `int argc` et `char *argv[]`.

```C++
#include<iostream>
using namespace std;

int main(int argc, char *argv[]) {
   for(int i = 0; i < argc; i++) {
      cout << "argv[" << i << "] = " << argv[i] << endl;
   }
   return 0;
}
```

Lorsque vous exécutez le programme avec des arguments en ligne de commande (par exemple `./programme arg1 arg2`), il affiche:

```C++
argv[0] = ./programme
argv[1] = arg1
argv[2] = arg2
```

## Plongée profonde

Historiquement, la lecture des arguments de la ligne de commande est couramment utilisée dans les langages de programmation bas niveau, car elle offre une plus grande flexibilité aux utilisateurs finaux. 

Concernant les alternatives, dans certaines situations, vous pouvez utiliser l'entrée standard (`stdin`) pour obtenir des entrées utilisateur pendant l'exécution du programme. Toutefois, les arguments de la ligne de commande sont plus appropriés pour les options de programme à court terme.

Le tableau `argv` est un tableau de pointeurs. Chaque élément (de `argv[0]` à `argv[n]`) est un pointeur vers une chaîne de caractères. Le nombre de chaînes (`argc`) est déterminé par le nombre d'arguments entrés en ligne de commande.

## Voir aussi

Voici des liens pratiques pour vous plonger dans le sujet:

- Guide pour les débutants sur l'interaction avec le système d'exploitation : https://www.learn-coding.org/
- Information plus détaillée sur les arguments de la ligne de commande: https://www.learncpp.com/cpp-tutorial/command-line-arguments/
- Comparaison entre entrée standard et arguments de la ligne de commande: https://stackoverflow.com/questions/366437/arguments-or-input-stream