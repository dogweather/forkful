---
title:    "C#: Affichage de la sortie de débogage"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage est un outil précieux pour tout programmeur en C#. Cela permet de visualiser les valeurs des variables et de suivre le flux de votre code pendant l'exécution. Cela peut également aider à détecter et à résoudre rapidement les erreurs.

## Comment faire

```C#
// Exemple de code pour imprimer une sortie de débogage
string nom = "Jean";
int age = 25;
int somme = 10 + 5;

// Imprimer les valeurs des variables
Console.WriteLine($"Le nom est {nom}");
Console.WriteLine($"L'âge est {age}");
Console.WriteLine($"La somme est {somme}");

// Sortie de débogage:
// Le nom est Jean
// L'âge est 25
// La somme est 15
```

La première étape pour imprimer une sortie de débogage est de choisir les valeurs des variables que vous souhaitez afficher. Ensuite, utilisez la méthode `Console.WriteLine()` pour imprimer ces valeurs dans la console. Il est important de noter que `Console.WriteLine()` utilise le formatage de chaîne pour inclure les valeurs des variables dans le message à imprimer.

Vous pouvez également utiliser la méthode `Console.Write()` pour imprimer les valeurs sans saut de ligne à la fin. Cela peut être utile si vous souhaitez imprimer plusieurs valeurs sur une seule ligne.

## Plongée profonde

Il est possible d'ajouter des informations supplémentaires à votre sortie de débogage en incluant des expressions booléennes et conditionnelles. Cela peut être utile pour imprimer des messages spécifiques en fonction de certaines conditions.

```C#
// Exemple de code avec expressions booléennes et conditionnelles
string ville = "Paris";
int temp = 25;

if (temp > 30)
{
    Console.WriteLine($"Il fait vraiment chaud à {ville} aujourd'hui.");
}
else if (temp < 20)
{
    Console.WriteLine($"Il fait un peu frais à {ville} aujourd'hui.");
}
else
{
    Console.WriteLine($"Il fait beau à {ville} aujourd'hui.");
}

// Sortie de débogage:
// Il fait beau à Paris aujourd'hui.
```

En utilisant des expressions booléennes et conditionnelles, vous pouvez personnaliser votre sortie de débogage en fonction du contexte de votre code. Cela peut vous aider à identifier les erreurs et à améliorer la compréhension de votre code.

## Voir aussi

- [Documentation Microsoft sur l'impression de sortie de débogage en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/io/how-to-write-to-the-debug-window)
- [Article sur la gestion des erreurs et des exceptions en C#](https://blog.goyello.com/2018/03/13/error-handling-and-exception-management-in-c-sharp/)