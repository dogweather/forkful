---
title:                "Travailler avec le json"
html_title:           "C#: Travailler avec le json"
simple_title:         "Travailler avec le json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données dans vos applications C#, il y a de fortes chances que vous ayez déjà entendu parler de JSON. Avec la montée en popularité des API web, JSON est devenu un format de données incontournable pour échanger des informations entre différents systèmes. Dans cet article, nous allons explorer pourquoi il est important de comprendre et de travailler avec JSON dans le contexte de la programmation en C#.

## Comment faire

Commençons par un exemple simple de création d'un objet JSON en utilisant le framework JSON.NET, l'une des bibliothèques les plus populaires pour travailler avec JSON en C#.

```C#
using Newtonsoft.Json;

// Définition d'un objet C#
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

// Création d'un objet Person
var person = new Person()
{
    Name = "Jean",
    Age = 30
};

// Sérialisation de l'objet en JSON
var json = JsonConvert.SerializeObject(person);

Console.WriteLine(json); // Output: {"Name":"Jean","Age":30}
```

Nous avons converti notre objet C# en une chaîne JSON en utilisant la méthode `JsonConvert.SerializeObject()`. On peut également désérialiser une chaîne JSON en un objet C# en utilisant la méthode `JsonConvert.DeserializeObject()`. Maintenant, voyons comment travailler avec des données plus complexes en utilisant JSON.

```C#
// Définition d'un objet C#
public class Book
{
    public string Title { get; set; }
    public string Author { get; set; }
    public string[] Genres { get; set; }
}

// Création d'un objet Book
var book = new Book()
{
    Title = "L'Étranger",
    Author = "Albert Camus",
    Genres = new string[] { "Philosophie", "Fiction" }
};

// Sérialisation de l'objet en JSON avec mise en forme
var json = JsonConvert.SerializeObject(book, Formatting.Indented);

Console.WriteLine(json);

/*
    Output:
    {
        "Title": "L'Étranger",
        "Author": "Albert Camus",
        "Genres": [
            "Philosophie",
            "Fiction"
        ]
    }
*/
```

Nous pouvons également utiliser JSON pour échanger des données avec des API web ou pour stocker des configurations dans nos applications. Le framework JSON.NET dispose de nombreuses fonctionnalités et options de configuration pour gérer ces cas d'utilisation plus avancés.

## Plongée en profondeur

Maintenant que nous avons vu comment travailler avec JSON en C#, voici quelques points à garder à l'esprit lors de l'utilisation de JSON dans vos projets :

- JSON est un format léger et facile à comprendre, mais il peut être facilement invalidé en cas de mauvaise manipulation. Utilisez les outils fournis par votre framework ou bibliothèque pour valider et formater correctement vos données JSON.
- Assurez-vous de bien connaître les conventions de nommage et d'utilisation des accolades pour délimiter les objets et les tableaux dans JSON. Cela facilitera grandement la lecture et la compréhension de votre code.
- Si vous travaillez avec des fichiers JSON volumineux, pensez à utiliser des bibliothèques de traitement en streaming pour éviter de charger en mémoire tout le contenu du fichier.

## Voir aussi

- [Documentation officielle JSON.NET](https://www.newtonsoft.com/json)
- [L'essentiel de JSON en C#](https://www.codeproject.com/Articles/363634/JSON-Serialization-and-Deserialization-in-Csharp)
- [Créer une API web avec JSON en C#](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/request-response?view=aspnetcore-5.0#json-request-body)