---
title:                "Travailler avec TOML"
aliases:
- /fr/java/working-with-toml/
date:                  2024-01-26T04:23:02.865909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML signifie Tom's Obvious, Minimal Language (Langage Minimal Évident de Tom). C'est un format de sérialisation de données utilisé pour les fichiers de configuration. Les programmeurs l'utilisent parce qu'il est facile à lire, à écrire, et se cartographie bien à une table de hachage.

## Comment faire :
Vous aurez besoin d'une bibliothèque d'analyse TOML. Je recommande `toml4j`. Ajoutez-le à votre projet ainsi :

```java
// Ajoutez ceci à votre build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Voici comment analyser un fichier TOML :

```java
import com.moandjiezana.toml.Toml;

public class ExempleToml {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [serveur]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("serveur.ip");
        Integer port = toml.getLong("serveur.port").intValue();
        
        System.out.println("IP du serveur : " + ip);
        System.out.println("Port du serveur : " + port);
    }
}
```

Exemple de sortie :

```
IP du serveur : 192.168.1.1
Port du serveur : 80
```

## Approfondissement
Développé par le co-fondateur de GitHub, Tom Preston-Werner, TOML vise à être plus simple que XML et plus spécifié que YAML. Sa dernière version 1.0.0, sortie en 2021, propose un ensemble stable de fonctionnalités.

Des alternatives telles que JSON ou YAML sont également populaires. JSON est excellent pour l'échange de données. YAML est plus lisible par l'homme pour des configurations complexes. La force de TOML réside dans sa simplicité et son utilisation dans la communauté Rust.

En ce qui concerne l'implémentation, lors de l'utilisation de TOML avec Java, gardez à l'esprit que le parseur que vous choisissez importe. Au-delà de `toml4j`, certains optent pour `jackson-dataformat-toml`. Chacun aura ses nuances, comme la gestion des erreurs ou la performance de l'analyse, donc choisissez en fonction des besoins de votre projet.

## Voir Aussi
- Spécification TOML : https://toml.io/fr/
- `toml4j` GitHub : https://github.com/mwanji/toml4j
- `jackson-dataformat-toml` : https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
