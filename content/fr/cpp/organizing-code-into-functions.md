---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:09:01.153070-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Découper le code en fonctions signifie diviser votre code en morceaux plus petits et réutilisables. Nous le faisons pour éviter la répétition, pour rendre notre code lisible et pour simplifier le débogage et les tests. Des fonctions bien organisées peuvent être comme avoir une boîte d'outils soigneusement étiquetés, prêts à l'emploi et à partager.

## Comment faire :
Prenons une tâche courante : calculer l'aire d'un cercle. Au lieu d'écrire la même formule à chaque fois, nous l'encapsulons dans une fonction.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Aire du cercle avec un rayon de " << r << " est " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Exemple de sortie :
```
Aire du cercle avec un rayon de 5 est 78.5397
```

## Exploration approfondie
Historiquement, les procédures et les fonctions étaient l'épine dorsale de la programmation structurée, promue dans les années 1960 pour combattre les problèmes de "code spaghetti" dans les langages de programmation impératifs antérieurs. Des alternatives comme la POO (Programmation Orientée Objet) vont plus loin en associant ces fonctions à des structures de données. En C++, vous avez des fonctions régulières, des méthodes de classe (y compris des méthodes statiques), des lambdas et des fonctions de template, chacune offrant différents avantages. La mise en œuvre de fonctions bien organisées implique généralement de respecter des principes tels que DRY ("Ne vous répétez pas") et SRP (Principe de Responsabilité Unique), ce qui signifie que chaque fonction ne fait qu'une chose et la fait bien.

## Voir aussi
Pour plus d'informations sur les fonctions en C++ :
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Pour les principes de conception liés aux fonctions :
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Apprenez-en davantage sur les lambdas et l'utilisation avancée des fonctions :
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures