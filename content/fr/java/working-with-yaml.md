---
title:                "Travailler avec le yaml"
html_title:           "Java: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un passionné de programmation, vous avez sûrement entendu parler de YAML. Mais qu'est-ce que c'est réellement et pourquoi devriez-vous l'utiliser ? YAML est un format de données léger, facile à lire et à écrire, utilisé pour représenter des données structurées. Il est souvent utilisé pour la configuration et le stockage de données dans les applications et offre une alternative plus simple et plus lisible que le format JSON.

## Comment faire

Pour utiliser YAML dans votre code Java, vous aurez besoin de la bibliothèque SnakeYAML. Voici un exemple de code pour lire un fichier YAML et en extraire des données :

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.*;

public class YAMLReader{
    public static void main(String[] args) throws FileNotFoundException {
        // créer un objet Yaml
        Yaml yaml = new Yaml();
        
        // lire le fichier YAML
        InputStream inputStream = new FileInputStream(new File("exemple.yml"));
        
        // parser le fichier et stocker les données dans une HashMap
        HashMap<String, Object> data = yaml.load(inputStream);
        
        // accéder aux données et les afficher
        System.out.println(data.get("nom"));
        System.out.println(data.get("âge"));
        System.out.println(data.get("ville"));
    }
}
```

Output :

```
John Doe
25
Paris
```

Vous pouvez également écrire des données dans un fichier YAML en utilisant la classe YamlWriter. Voici un exemple :

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.*;

public class YAMLWriter{
    public static void main(String[] args) throws FileNotFoundException {
        // créer un objet Yaml
        Yaml yaml = new Yaml();
        
        // créer une HashMap avec des données à écrire
        HashMap<String, Object> data = new HashMap<>();
        data.put("nom", "Jane Doe");
        data.put("âge", 30);
        data.put("ville", "Lyon");
        
        // écrire les données dans un fichier YAML
        OutputStream outputStream = new FileOutputStream(new File("nouveau.yml"));
        yaml.dump(data, new OutputStreamWriter(outputStream));
    }
}
```

Output (dans un fichier nommé "nouveau.yml") :

```
nom: Jane Doe
âge: 30
ville: Lyon
```

## Plongée en profondeur

Maintenant que vous savez comment lire et écrire des données avec YAML en Java, voici quelques points à garder à l'esprit :

- Les données YAML peuvent être organisées en listes, en dictionnaires ou en objets, offrant une grande flexibilité.
- Il est important de respecter l'indentation lors de l'écriture de données YAML, car cela détermine la structure et la hiérarchie des données.
- La bibliothèque SnakeYAML prend également en charge la validation de schémas YAML pour s'assurer que les données sont dans le format attendu.

Maintenant que vous avez les bases, n'hésitez pas à explorer les nombreuses fonctionnalités de YAML et à l'utiliser dans vos projets pour une gestion de données plus simple et plus claire.

## Voir aussi

- [Documentation officielle de SnakeYAML](https://bitbucket.org/asomov/snakeyaml)
- [Format YAML vs JSON](https://stackabuse.com/yaml-vs-json-which-is-the-better-configuration-format/)