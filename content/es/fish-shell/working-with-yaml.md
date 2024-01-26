---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

El YAML es un formato de serialización de datos legible por humanos, usado frecuentemente para los archivos de configuración. Los programadores lo utilizan por su simplicidad y facilidad de leer, especialmente en configuraciones complejas y proyectos que involucran múltiples lenguajes de programación.

## Cómo:

En Fish Shell, no hay herramientas incorporadas para manejar YAML de manera nativa, pero puedes usar `yq`, una herramienta de línea de comandos basada en `jq`. Primero, instala `yq` usando Homebrew:

```Fish Shell
brew install yq
```

Luego, puedes leer un archivo YAML:

```Fish Shell
yq e '.raíz.nodo' archivo.yaml
```

O modificar un archivo YAML:

```Fish Shell
yq e '.raíz.nodo = "valorNuevo"' -i archivo.yaml
```

Comandos y salida de ejemplo para agregar un elemento a una lista:

```Fish Shell
echo -e "frutas:\n  - manzana\n  - banana" > frutas.yaml
yq e '.frutas += "naranja"' -i frutas.yaml
cat frutas.yaml
```

Salida:

```YAML
frutas:
  - manzana
  - banana
  - naranja
```

## Profundización

El YAML fue creado en 2001 y es un acrónimo para "YAML Ain't Markup Language". El diseño enfatiza la facilidad de lectura y soporta estructuras de datos como listas, mapas y escalares. Aunque JSON y XML son alternativas populares, YAML es preferido en ciertos entornos debido a su concisión y legibilidad. El uso correcto de `yq` y su integración en Fish Shell puede simplificar enormemente la manipulación de archivos YAML en múltiples contextos.

## Ver También

- Documentación oficial de `yq`: [https://mikefarah.gitbook.io/yq/](https://mikefarah.gitbook.io/yq/)
- YAML 1.2 especificación oficial: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Introducción a YAML para principiantes: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
