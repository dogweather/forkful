---
title:                "Impression de sortie de débogage"
html_title:           "C#: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font-ils? 
L'affichage des informations de débogage, également appelé "printf debugging" en référence à la fonction printf en C, est une technique couramment utilisée par les programmeurs pour comprendre le comportement d'un programme en affichant du texte ou des valeurs de variables lors de l'exécution du code. Cela peut aider à localiser et à résoudre les erreurs de façon plus efficace.

## Comment faire: 
Voici un exemple en utilisant la console de sortie en C# pour afficher du texte et les valeurs de variables :

```C#
string nom = "Bob";
int age = 35;
Console.WriteLine("Bonjour, je m'appelle " + nom);
Console.WriteLine("J'ai " + age + " ans");
```

La sortie de ce code serait la suivante : 
```
Bonjour, je m'appelle Bob
J'ai 35 ans
```

## Plongée en profondeur: 
Bien que l'affichage des informations de débogage soit souvent considéré comme une technique de débogage rudimentaire, cela reste un outil utile dans certaines situations. Avant l'apparition des debuggers, l'affichage de valeurs de variables était le seul moyen de comprendre le comportement du code. Aujourd'hui, il existe des alternatives plus avancées telles que le débogage pas à pas avec un debugger ou l'utilisation de points d'arrêt. Cependant, afficher des informations de débogage reste une méthode simple et efficace pour comprendre rapidement le fonctionnement d'un programme.

## Voir aussi: 
Pour en savoir plus sur l'affichage des informations de débogage en C#, vous pouvez consulter la documentation officielle de Microsoft sur la console de sortie : https://docs.microsoft.com/fr-fr/dotnet/standard/io/troubleshoot-debug-console. Vous pouvez également trouver des conseils et astuces sur les techniques de débogage dans le livre "The Pragmatic Programmer" de Andrew Hunt et David Thomas.