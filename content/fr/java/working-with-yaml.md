---
title:                "Travailler avec yaml"
html_title:           "Java: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML en Java : Tout ce que vous devez savoir

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent ?

Le YAML est un format de données simple et lisible par l'homme, souvent utilisé pour stocker des configurations ou des données structurées. Les programmeurs l'utilisent principalement pour sa simplicité et sa flexibilité, ce qui en fait un choix populaire dans de nombreux projets en Java.

## Comment faire :

Voici un exemple de code Java montrant comment lire un fichier YAML et accéder à ses données :

```Java
// Importer la librairie
import org.yaml.snakeyaml.Yaml;

// Lire le fichier YAML
Yaml yaml = new Yaml();
InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream("config.yaml");
Map<String, Object> data = yaml.load(inputStream);

// Accéder aux données
String version = (String) data.get("version");
int port = (int) data.get("port");
List<String> database = (List) data.get("database");
```

La sortie de ce code pourrait ressembler à ceci :

```Java
version: 1.0
port: 8080
database:
  - MySQL
  - MongoDB
```

## Plongeons plus en profondeur :

Le format YAML a été créé en 2001 et est devenu de plus en plus populaire en raison de sa lisibilité et de sa compatibilité multi-langages. Pour ceux qui préfèrent une approche plus orientée objet, il existe également des alternatives telles que Jackson et Gson.

Pour implémenter le support YAML dans vos projets Java, vous pouvez utiliser la librairie SnakeYAML ou JAXB (Java Architecture for XML Binding). SnakeYAML est considérée comme plus rapide et plus légère, tandis que JAXB offre une meilleure intégration avec le framework Spring.

## À voir également :

- [Site officiel de YAML](https://yaml.org/)
- [La librairie SnakeYAML](https://bitbucket.org/asomov/snakeyaml)
- [La librairie JAXB](https://www.oracle.com/java/technologies/jaxb.html)

Fini les configurations complexes et les données illisibles ! Le YAML en Java vous permet de travailler avec des données structurées de manière simple et efficace. N'hésitez pas à l'essayer dans vos prochains projets.