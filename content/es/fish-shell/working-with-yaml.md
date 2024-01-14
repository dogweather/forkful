---
title:                "Fish Shell: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con YAML en Fish Shell?

YAML es un lenguaje de formato de datos con una sintaxis sencilla y legible para los humanos. Muchos programas, incluyendo Fish Shell, utilizan YAML para su configuración y datos estructurados. Aprender a utilizar YAML en Fish Shell te permitirá personalizar tus configuraciones y automatizar tareas de manera más eficiente.

## Cómo trabajar con YAML en Fish Shell

Para trabajar con YAML en Fish Shell, necesitas tener instalado el paquete "yaml" utilizando el administrador de paquetes Oh My Fish. Desde la terminal, ejecuta el siguiente comando:

```Fish Shell
omf install yaml
```

Una vez que el paquete está instalado, puedes utilizar los siguientes comandos para empezar a trabajar con YAML:

```Fish Shell
yaml convert <archivo_entrada> <archivo_salida>               # convierte un archivo YAML a formato JSON o viceversa
yaml lint <archivo>                                         # verifica la sintaxis de un archivo YAML
yaml get <archivo> <ruta>                                    # obtiene el valor de una clave dentro de un archivo YAML
yaml set <archivo> <ruta> <valor>                            # establece el valor de una clave dentro de un archivo YAML
yaml delete <archivo> <ruta>                                 # elimina una clave y su valor dentro de un archivo YAML
```

Ejemplo de archivo YAML:

```YAML
nombre: Juan
edad: 35
hobbies:
  - leer
  - cocinar
  - viajar
```

Ejemplo de salida JSON después de convertir el archivo anterior:

```JSON
{
  "nombre": "Juan",
  "edad": 35,
  "hobbies": [
    "leer",
    "cocinar",
    "viajar"
  ]
}
```

## Profundizando en el uso de YAML en Fish Shell

Además de los comandos mencionados anteriormente, Fish Shell también ofrece una sintaxis fácil de usar para trabajar con YAML. Puedes crear y editar archivos YAML directamente en la terminal utilizando el siguiente formato:

```Fish Shell
set -y <ruta> <valor>
```

Por ejemplo, podemos crear el mismo archivo YAML del ejemplo anterior de la siguiente manera:

```Fish Shell
set -y nombre "Juan"
set -y edad 35
set -y hobbies[1] "leer"
set -y hobbies[2] "cocinar"
set -y hobbies[3] "viajar"
```

Esto nos permite automatizar tareas más complejas como la creación de múltiples archivos YAML, modificación de valores en archivos existentes, entre otros.

Si deseas profundizar aún más en el uso de YAML en Fish Shell, puedes consultar la documentación oficial del paquete y aprender más sobre las opciones disponibles para cada comando.

## Ver también

- [Documentación oficial del paquete YAML en Fish Shell](https://github.com/oh-my-fish/plugin-yaml)
- [Tutorial de YAML en Fish Shell](https://medium.com/@Crisda_/crea-y-edita-tus-archivos-yaml-desde-fish-shell-ccc77bf7f1a4)
- [Introducción a YAML para desarrolladores](https://docs.ansible.com/ansible/YAMLSyntax.html)