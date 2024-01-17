---
title:                "Trabajando con yaml"
html_title:           "Bash: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con YAML es una forma conveniente de organizar y almacenar datos de manera estructurada en Bash. Los programadores utilizan YAML para facilitar la lectura y mantenimiento de grandes cantidades de datos, ya que presenta información de manera legible y fácil de entender.

## ¿Cómo hacerlo?

Aquí hay un ejemplo sencillo de cómo trabajar con YAML en Bash:

```Bash
# Definir un archivo YAML
archivo_yaml="
nombre: Juan
edad: 25
país: México
"

# Leer los datos del archivo YAML
nombre=$(echo "$archivo_yaml" | awk '$1 == "nombre:" { print $2 }')
edad=$(echo "$archivo_yaml" | awk '$1 == "edad:" { print $2 }')
país=$(echo "$archivo_yaml" | awk '$1 == "país:" { print $2 }')

# Imprimir los datos
echo "Hola, mi nombre es $nombre y tengo $edad años. Soy de $país."
```

**Salida:**

```
Hola, mi nombre es Juan y tengo 25 años. Soy de México.
```

## Profundizando

YAML fue desarrollado originalmente para Ruby en 2001, pero hoy en día es ampliamente utilizado en muchos lenguajes de programación, incluyendo Bash. Es una alternativa popular a formatos de datos más complejos como JSON o XML. Para trabajar con YAML en Bash, se pueden utilizar diversas herramientas, como parsers y librerías específicas.

## Ver también

- [Sitio oficial de YAML](https://yaml.org/)
- [Tutorial básico sobre YAML en Bash](https://www.baeldung.com/yaml-bash)