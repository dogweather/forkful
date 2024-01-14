---
title:                "C#: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur logiciel, vous avez probablement déjà entendu parler de YAML. Mais pourquoi est-ce important? Pourquoi devriez-vous vous intéresser à travailler avec YAML? La réponse est simple: YAML est un format de sérialisation de données populaire utilisé dans de nombreux projets de développement. C'est un outil utile pour stocker et transmettre des données de manière structurée et facile à lire. 

## Comment faire

Si vous souhaitez commencer à travailler avec YAML en utilisant C#, voici quelques exemples de code pour vous aider. 

```C#
// Exemple de création d'un fichier YAML 
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Personne
{
    public string Nom { get; set; }
    public string Prenom { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        var personne = new Personne
        {
            Nom = "Dupont",
            Prenom = "Jean",
            Age = 31
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance)
            .Build();

        var yaml = serializer.Serialize(personne);
        File.WriteAllText("personne.yaml", yaml);
    }
}

```

```C#
// Exemple de lecture d'un fichier YAML
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Personne
{
    public string Nom { get; set; }
    public string Prenom { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        var input = new StringReader(File.ReadAllText("personne.yaml"));
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance)
            .Build();

        var personne = deserializer.Deserialize<Personne>(input);
        Console.WriteLine("Nom: {0}, Prenom: {1}, Age: {2}", personne.Nom, personne.Prenom, personne.Age);
    }
}
```

Output: 
```
Nom: Dupont, Prenom: Jean, Age: 31
```

## Plongée en profondeur

Maintenant que vous avez vu comment créer et lire un fichier YAML en utilisant C#, il est temps de plonger plus en profondeur dans les fonctionnalités de YAML. Voici quelques éléments clés à prendre en compte : 

- YAML est basé sur une syntaxe simple et facile à lire, ce qui le rend idéal pour stocker et transférer des données entre différentes applications.
- YAML prend en charge les commentaires, ce qui peut être utile pour la documentation ou pour ajouter des informations supplémentaires à vos données.
- Il est possible d'utiliser des références dans YAML pour éviter les redondances de données.
- YAML offre une grande flexibilité avec ses types de données, tels que les tableaux, les chaînes de caractères, les nombres, les booléens, etc.

Pour en savoir plus sur YAML, n'hésitez pas à consulter la section de documentation officielle sur la syntaxe YAML pour de plus amples informations.

## Voir aussi

- [Documentation officielle sur la syntaxe YAML](https://yaml.org/spec/1.2/spec.html)
- [YAML en 5 minutes](https://learnxinyminutes.com/docs/yaml/)
- [Bibliothèque YamlDotNet pour C#](https://github.com/aaubry/YamlDotNet)