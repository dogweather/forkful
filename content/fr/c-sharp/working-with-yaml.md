---
title:                "Travailler avec le yaml."
html_title:           "C#: Travailler avec le yaml."
simple_title:         "Travailler avec le yaml."
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi 

Vous avez probablement déjà entendu parler de YAML si vous travaillez avec du code en C#. Mais pourquoi est-il si important d'en savoir plus sur YAML ? Eh bien, YAML est un format de données configurable facile à lire et à écrire, ce qui le rend idéal pour la communication entre différentes applications.

## Comment faire 

La première étape pour travailler avec YAML en C# est d'installer le package NuGet "YamlDotNet". Ensuite, vous pouvez commencer à manipuler vos données en utilisant les classes de ce package. Voici un exemple de code montrant comment lire et écrire des données YAML :

```C#
using System;
using YamlDotNet.Serialization;
using System.IO;

namespace YAMLExample
{
    class Program
    {
        static void Main(string[] args)
        {
            var data = new
            {
                Name = "John",
                Age = 25,
                Hobbies = new string[] { "reading", "coding", "cooking" }
            };

            // Writing data to YAML file
            string yaml = new SerializerBuilder().Build().Serialize(data);
            File.WriteAllText("data.yaml", yaml);

            // Reading data from YAML file
            var deserializer = new DeserializerBuilder().Build();
            var result = deserializer.Deserialize<Person>(File.OpenText("data.yaml").ReadToEnd());

            Console.WriteLine("Name: " + result.Name);
            Console.WriteLine("Age: " + result.Age);
            Console.WriteLine("Hobbies: ");
            foreach (var hobby in result.Hobbies)
            {
                Console.WriteLine("- " + hobby);
            }
        }
    }

    // Defining a person class to deserialize YAML data to
    class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string[] Hobbies { get; set; }
    }
}
```

Voici le résultat de l'exécution du code ci-dessus : 

```
Name: John
Age: 25
Hobbies:
- reading
- coding
- cooking
```

## Plongée Plus Profonde 

Maintenant que vous savez comment lire et écrire des données YAML en utilisant C#, vous pouvez explorer d'autres fonctionnalités offertes par le package "YamlDotNet". Par exemple, vous pouvez utiliser des attributs pour personnaliser la sérialisation de vos données. Vous pouvez également utiliser des convertisseurs personnalisés pour prendre en charge des types de données complexes.

## Voir Aussi 

- [Documentation officielle de YamlDotNet](https://www.nuget.org/packages/YamlDotNet/)
- [Tutoriel complet sur YAML en C#](https://dotnetcoretutorials.com/2021/04/01/working-with-yaml-in-c/)