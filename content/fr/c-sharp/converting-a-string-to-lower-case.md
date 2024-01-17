---
title:                "Conversion d'une chaîne de caractères en minuscules"
html_title:           "C#: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La conversion d'une chaîne de caractères en minuscules est une opération fréquemment utilisée en programmation pour uniformiser l'apparence des données textuelles. Cela signifie que toutes les lettres de la chaîne seront changées en minuscules. Les programmeurs utilisent cette méthode pour faciliter la comparaison de chaînes et améliorer la lisibilité du code.

## Comment le faire:

```C#
string myString = "HELLO WORLD";
Console.WriteLine(myString.ToLower());
```
**Output:** hello world

La méthode ToLower() peut être appliquée à n'importe quelle chaîne de caractères et renvoie une nouvelle chaîne avec toutes les lettres en minuscules. Le paramètre CultureInfo peut également être utilisé pour spécifier la culture à utiliser pour la conversion.

## Plongée en profondeur:

Cette méthode a été introduite dans le cadre du standard Unicode en 1991, ce qui a permis de traiter les chaînes de caractères multilingues de manière uniforme. Avant cela, chaque langue avait ses propres règles de conversion, ce qui compliquait le traitement des données textuelles au niveau international.

Une alternative à la conversion en minuscules est la conversion en majuscules avec la méthode ToUpper(), qui peut également être utile pour certains cas d'utilisation. Toutefois, il est important de noter que la conversion en majuscules peut entraîner la perte d'informations dans certaines langues où les lettres majuscules ont une signification différente des lettres minuscules (par exemple, en allemand).

Au niveau de l'implémentation, la méthode ToLower() utilise les règles de casse des langues spécifiées dans les paramètres de culture pour déterminer comment chaque lettre doit être convertie en minuscule. Cela peut varier en fonction de la langue et du système d'exploitation utilisé.

## Voir aussi:

- [Documentation officielle de Microsoft sur la méthode ToLower() en C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [Tutorialspoint sur la conversion de chaînes en minuscules en C#](https://www.tutorialspoint.com/csharp-program-to-convert-string-to-lowercase)