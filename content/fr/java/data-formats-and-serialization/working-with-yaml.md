---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:43.337495-07:00
description: "YAML, acronyme de \"YAML Ain't Markup Language\" (YAML n'est pas un\
  \ langage de balisage), est un standard de s\xE9rialisation de donn\xE9es lisible\
  \ par l'humain\u2026"
lastmod: '2024-03-13T22:44:57.665474-06:00'
model: gpt-4-0125-preview
summary: "YAML, acronyme de \"YAML Ain't Markup Language\" (YAML n'est pas un langage\
  \ de balisage), est un standard de s\xE9rialisation de donn\xE9es lisible par l'humain\
  \ que les programmeurs utilisent pour les fichiers de configuration, le vidage de\
  \ donn\xE9es et la transmission de donn\xE9es entre langues."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
En Java, vous pouvez travailler avec des fichiers YAML en utilisant des bibliothèques tierces puisque l'Édition Standard de Java n'inclut pas de support intégré pour YAML. Une bibliothèque populaire est SnakeYAML, qui permet de parser et de générer des données YAML facilement.

### Configurer SnakeYAML
Tout d'abord, incluez SnakeYAML dans votre projet. Si vous utilisez Maven, ajoutez la dépendance suivante à votre `pom.xml` :

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Lire du YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
En supposant que `config.yml` ressemble à ceci :
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
Le résultat sera :
```
{name=Example, version=1.0, features=[login, signup]}
```

### Écrire du YAML
Pour générer un YAML à partir d'objets Java, utilisez la méthode `dump` fournie par SnakeYAML :
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
Cela générera et affichera le contenu YAML suivant :
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
En tirant parti de SnakeYAML, les développeurs Java peuvent facilement intégrer l'analyse et la génération YAML dans leurs applications, bénéficiant de la lisibilité et de la simplicité de YAML à des fins de configuration et d'échange de données.
