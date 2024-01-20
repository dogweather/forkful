---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Chercher et remplacer du texte en C#
## Qu'est-ce que c'est et pourquoi?
Le recherche et le remplacement de texte est un processus de localisation de séquences de caractères spécifiques dans une chaîne de texte et de remplacement de ces séquences par d'autres. Les programmeurs font cela pour modifier le contenu des textes, corriger les erreurs ou mettre à jour les informations.
## Comment faire :
Voici une méthode simple pour rechercher et remplacer du texte en C# :
```C#
string texte = "Bonjour tout le monde!";
string chercher= "le monde";
string remplacer = "à tous";

string texteMisÀJour = texte.Replace(chercher, remplacer);
Console.WriteLine(texteMisÀJour);
```
La sortie de ce programme sera :
```C#
"Bonjour à tous!"
```
## En profondeur:
(1) **Contexte historique** : Le remplacement de texte est un concept aussi ancien que la programmation elle-même. En C#, la fonction Replace() fait partie de la bibliothèque de classes de base .NET depuis sa première version en 2002.

(2) **Alternatives** : Il existe également d'autres méthodes pour remplacer du texte en C#, par exemple en utilisant l'expression régulière (RegEx), qui offre plus de flexibilité et de contrôle.

(3) **Détails de mise en œuvre** : La fonction Replace() de C# utilise en interne un algorithme de balayage de gauche à droite. Elle parcourt la chaîne caractère par caractère, cherchant la séquence qui correspond à la chaîne à remplacer. Lorsqu'elle la trouve, elle remplace la séquence par la chaîne de remplacement.

## Voir aussi :
- [Documentation Microsoft sur `String.Replace Method`](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.replace?view=net-5.0)
- [Guide de Microsoft sur l'utilisation des expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions)