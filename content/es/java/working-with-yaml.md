---
title:                "Trabajando con yaml"
html_title:           "Java: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con YAML es una forma de estructurar datos en formato de texto legible para humanos. Los programadores lo utilizan para almacenar y compartir información de manera organizada y fácilmente interpretable por otros equipos o sistemas.

## ¿Cómo hacerlo?

Para trabajar con YAML en Java, es necesario importar la librería SnakeYAML y utilizar sus métodos para inicializar, escribir y leer archivos YAML. A continuación se muestra un ejemplo de cómo crear un archivo YAML y leer su contenido:

```
import org.yaml.snakeyaml.*;

// Inicializar objeto YAML
Yaml yaml = new Yaml();

// Crear archivo y escribir datos
yaml.dump("nombre: Juan, edad: 28", new FileWriter("datos.yml"));

// Leer archivo y almacenar datos en un mapa
Map<String, Object> datosMap = yaml.load(new FileReader("datos.yml"));

// Imprimir valores individuales
System.out.println(datosMap.get("nombre")); // Salida: Juan
System.out.println(datosMap.get("edad")); // Salida: 28
```

## Profundizando

El formato YAML fue creado en 2001 como una alternativa al formato XML más complejo y difícil de leer. Aunque se parece al formato JSON, YAML se enfoca en ser más legible para humanos y permite comentarios y referencias a otros objetos dentro del mismo archivo.

Algunas alternativas para trabajar con datos estructurados en Java incluyen XML y JSON, pero YAML ha ganado popularidad por su simplicidad y capacidad de ser leído y modificado fácilmente por personas.

La implementación de SnakeYAML en Java se basa en la especificación de YAML 1.1 y es compatible con los estándares de Java para trabajar con mapas y objetos.

## Ver también

- Página oficial de SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Especificación de YAML 1.1: https://yaml.org/spec/1.1/
- Ejemplos de YAML: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html