---
title:                "Écriture de tests"
html_title:           "C#: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Ecrire des tests est une pratique courante pour les programmeurs afin de s'assurer que leur code fonctionne correctement et de détecter rapidement d'éventuelles erreurs ou bugs. Les tests permettent également de faciliter la maintenance du code en identifiant les parties qui nécessitent des modifications.

## Comment:
Voici un exemple de code dans lequel nous allons tester une fonction qui calcule le carré d'un nombre :

```C#
int carre = CalculerCarre(5);
Console.WriteLine(carre);
```
Le résultat affiché sera `25`, car la fonction `CalculerCarre()` prend en paramètre un entier et renvoie sa valeur au carré.

## Plongée en profondeur:
L'écriture de tests automatisés est devenue une pratique de plus en plus importante dans le développement logiciel. Elle permet de valider chaque fonction individuellement et d'assurer que le code répond aux exigences spécifiques. Cela peut également aider à repérer facilement des régressions dans le code lorsque de nouvelles modifications sont apportées.

Il existe également des alternatives à l'écriture de tests automatisés, telles que les tests manuels, qui sont effectués manuellement par un testeur humain. Cependant, ces tests peuvent être plus coûteux en temps et en ressources et ne sont pas aussi précis que les tests automatisés.

L'implémentation de tests en C# peut être réalisée en utilisant des frameworks tels que NUnit, xUnit ou Microsoft Visual Studio Test Platform. Ces frameworks offrent des fonctionnalités pour faciliter l'écriture et l'exécution de tests automatisés.

## Voir aussi:
- [Introduction to Automated Software Testing](https://www.guru99.com/automated-testing.html)
- [xUnit.net](https://xunit.net/)
- [NUnit](https://nunit.org/)
- [Microsoft Visual Studio Test Platform](https://docs.microsoft.com/en-us/visualstudio/test/overview-of-the-visual-studio-test-platform?view=vs-2019)