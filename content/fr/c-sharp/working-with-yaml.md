---
title:                "Travailler avec le yaml"
html_title:           "C#: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?

Le YAML est un format de données utilisé par les programmeurs pour stocker des informations de configuration. Les développeurs utilisent le YAML pour définir des structures de données facilement lisibles par les humains et les machines.

## Comment faire:

Voici un exemple de code montrant comment utiliser le YAML en C#:

```C#
using System;
using System.IO;
using YamlDotNet.RepresentationModel;

class Program
{
    static void Main(string[] args)
    {
        // Ouverture du fichier YAML
        var input = new StreamReader("config.yaml");
        
        // Chargement des données en utilisant la librairie YamlDotNet
        var yaml = new YamlStream();
        yaml.Load(input);
        
        // Récupération des données sous forme de dictionnaire
        var mapping = (YamlMappingNode)yaml.Documents[0].RootNode;
        
        // Affichage de la valeur associée à la clé "nom"
        Console.WriteLine((string)mapping.Children[new YamlScalarNode("nom")]);
    }
}
```

Le fichier `config.yaml` serait alors le suivant:

```yaml
nom: John Doe
age: 30
pays: France
```

Et la sortie du programme serait `John Doe`.


## Plongée en profondeur:

Le YAML a été créé en 2001 par Clark Evans avec l'aide de Ingy döt Net, Oren Ben-Kiki et d'autres contributeurs. Il a été conçu pour être un format facile à lire pour les humains tout en étant facile à utiliser pour les machines.

Il existe d'autres formats de données populaires tels que JSON et XML qui peuvent être utilisés à la place du YAML. Cependant, le YAML offre un certain nombre d'avantages tels que sa syntaxe plus concise et sa capacité à inclure d'autres structures de données telles que les tableaux et les dictionnaires. 

Pour mettre en place le YAML dans un projet C#, il est possible d'utiliser des librairies telles que YamlDotNet ou SharpYaml.

## Voir aussi:

- Site officiel du YAML: <https://yaml.org/>
- YamlDotNet: <https://github.com/aaubry/YamlDotNet>
- SharpYaml: <https://github.com/xoofx/SharpYaml>