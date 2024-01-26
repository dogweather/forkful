---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, c'est simple: des données en texte, facile à lire. Les devs l'utilisent pour des configs, parce que c'est clair et rapide à piger.

## How to:
Pour bosser avec YAML en Java, la lib snakeyaml est nickel. Voilà du code pour lire un fichier YAML:

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = ReadYamlExample.class
            .getClassLoader()
            .getResourceAsStream("config.yaml") ;
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```

Si t'as un `config.yaml` qui ressemble à ça:

```YAML
database:
  host: localhost
  port: 3306
```

Le programme va afficher: `{database={host=localhost, port=3306}}`. Facile, hein?

## Deep Dive
YAML débarque dans les années 2000, alternative à XML, trop verbeux. JSON suit, mais YAML reste royal pour sa lisibilité. Snakeyaml ? Une des implémentations côté Java. Attention aux injections YAML (sécurité!), et si t'as besoin, y a aussi Jackson ou YamlBeans.

## See Also
Jette un œil ici pour plus de détails:
- Snakeyaml sur GitHub: https://github.com/asomov/snakeyaml
- Tutorial Snakeyaml: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- YAML officiel: https://yaml.org
- Pourquoi YAML: https://www.redhat.com/sysadmin/yaml-beginners
