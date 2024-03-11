---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:15.775163-07:00
description: "Travailler avec JSON (JavaScript Object Notation) signifie manipuler\
  \ ce format l\xE9ger d'\xE9change de donn\xE9es au sein de vos applications Java.\
  \ Les\u2026"
lastmod: '2024-03-11T00:14:31.618260-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec JSON (JavaScript Object Notation) signifie manipuler ce\
  \ format l\xE9ger d'\xE9change de donn\xE9es au sein de vos applications Java. Les\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec JSON (JavaScript Object Notation) signifie manipuler ce format léger d'échange de données au sein de vos applications Java. Les programmeurs optent pour JSON pour sérialiser et transmettre des données structurées via un réseau et configurer et stocker facilement des données car il est lisible par l'homme et indépendant du langage.

## Comment faire :
Retroussons nos manches et commençons à coder avec JSON en Java.

Première chose, vous aurez besoin d'une bibliothèque de traitement JSON comme `Jackson` ou `Google Gson`. Ici, nous utiliserons `Jackson`, donc ajoutez cette dépendance à votre `pom.xml` :

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Maintenant, sérialisons (écrivons) un simple objet Java en JSON :

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

Le résultat devrait être :

```json
{"name":"Alex","age":30}
```

Maintenant, pour désérialiser (lire) le JSON en un objet Java :

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " a " + person.age + " ans.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Le résultat sera :

```
Alex a 30 ans.
```

## Approfondissement
La simplicité et l'efficacité de JSON l'ont rendu le standard de facto pour l'échange de données sur le web, détrônant XML de son trône. Introduit au début des années 2000, JSON a été dérivé de JavaScript mais est maintenant pris en charge par la plupart des langages.

Les alternatives à JSON incluent XML, qui est plus verbeux, et des formats binaires comme Protocol Buffers ou MessagePack, qui sont moins lisibles par l'homme mais plus efficaces en taille et en vitesse. Chacun a ses cas d'utilisation ; le choix dépend de vos besoins spécifiques en données et du contexte.

En Java, au-delà de `Jackson` et `Gson`, nous avons `JsonB` et `org.json` comme autres bibliothèques pour gérer JSON. Jackson propose un traitement basé sur le flux et est connu pour sa vitesse, tandis que Gson est apprécié pour sa facilité d'utilisation. JsonB fait partie de Jakarta EE, offrant une approche plus standardisée.

Lors de la mise en œuvre de JSON, n'oubliez pas de gérer correctement vos exceptions - votre code doit être robuste contre les mauvaises entrées. Aussi, considérez les implications de sécurité de la liaison automatique de données – validez toujours vos entrées !

## Voir également
- [Projet Jackson](https://github.com/FasterXML/jackson)
- [Projet Gson](https://github.com/google/gson)
- [Spécification JSON](https://www.json.org/json-en.html)
- [Spécification JsonB](https://jakarta.ee/specifications/jsonb/)
