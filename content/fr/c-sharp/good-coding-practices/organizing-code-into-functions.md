---
title:                "Organisation du code en fonctions"
aliases:
- fr/c-sharp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:41.385527-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Regrouper le code en fonctions, c'est comme trier des briques LEGO dans des bacs – cela facilite leur recherche et leur utilisation. Nous faisons cela pour éviter la répétition, pour simplifier la compréhension, et pour rendre la maintenance moins pénible.

## Comment faire :
Imaginez que vous ayez du code qui affiche une salutation plusieurs fois. Sans fonctions, c'est un désordre. Avec des fonctions, c'est soigné.

```C#
// Sans fonctions - répétitif
Console.WriteLine("Bonjour, Amy !");
Console.WriteLine("Bonjour, Bob !");
Console.WriteLine("Bonjour, Charlie !");

// Avec fonctions - plus propre
void Saluer(string nom) {
    Console.WriteLine($"Bonjour, {nom} !");
}

Saluer("Amy");
Saluer("Bob");
Saluer("Charlie");
```

Le résultat est le même, mais la seconde version est bien plus ordonnée.

## Exploration approfondie
Autrefois, à l'époque du langage assembleur, on se déplaçait vers différents endroits du code avec GOTO – chaotique et difficile à suivre. Les fonctions représentent une amélioration majeure, comme des tiroirs organisés dans une boîte à outils. Des alternatives ? Bien sûr. Il y a les méthodes, qui sont des fonctions dans un contexte de classe. Puis il y a les lambda et les fonctions en ligne pour des tâches rapides et ponctuelles.

Concernant l'implémentation – des fonctions petites et ciblées sont en or. Elles sont plus faciles à tester et à déboguer. Les grandes fonctions avec de nombreuses responsabilités peuvent devenir monstrueuses, obtenant le titre peu enviable de "code spaghetti". Limitez-vous à une tâche par fonction ; vous vous en remercierez plus tard.

## Voir aussi
Pour plus d'informations sur les fonctions et les meilleures pratiques, consultez :

- Clean Code de Robert C. Martin : Principes pour garder vos fonctions ordonnées.
- Refactoring de Martin Fowler : Moyens d'améliorer le code existant.
- Guide Microsoft C# sur les méthodes : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/classes-and-structs/methods
