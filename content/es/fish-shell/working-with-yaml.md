---
title:                "Trabajando con yaml"
html_title:           "Fish Shell: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con YAML es una forma de estructurar y almacenar datos que es popular entre los programadores ya que es fácil de leer y escribir, y es compatible con una amplia variedad de lenguajes de programación.

## Cómo hacerlo:
Fish Shell tiene un módulo incorporado para trabajar con YAML llamado "yq". Puedes utilizarlo para leer y escribir archivos YAML, así como para convertir datos de YAML a otros formatos como JSON. Aquí hay un ejemplo de cómo leer un archivo YAML y crear una tabla con los datos utilizando Fish Shell:

```Fish Shell
#!/usr/bin/fish

set data (yq read sample.yml)

for key in (yq -P eval "keys" $data)
    printf "%s\t%s\n" $key (yq -P eval ".[\"$key\"]" $data)
end
```

El resultado de este código sería una tabla impresa en la consola con las claves y valores del archivo YAML.

## Inmersión profunda:
YAML fue creado en 2001 por el fundador de Ruby, Ingy döt Net, como una alternativa más human-readable al formato de datos JSON. Aunque YAML es ampliamente utilizado, también hay otras opciones para trabajar con datos estructurados, como XML y CSV.

Fish Shell también tiene otra herramienta integrada llamada "jq" que puede ser utilizada para trabajar con JSON. Además, puedes encontrar otras herramientas de terceros que pueden facilitar la manipulación de datos YAML en Fish Shell.

## Véase también:
- [Página oficial de YAML](https://yaml.org/)