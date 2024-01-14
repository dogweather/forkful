---
title:                "Bash: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando una forma fácil de organizar y almacenar datos de forma legible y estructurada, trabajar con YAML podría ser la solución perfecta para ti. Además, ¡es compatible con una variedad de lenguajes de programación y herramientas como Bash!

## Cómo hacerlo

Para empezar, debes tener en cuenta que YAML (YAML Ain't Markup Language) es un formato para representar datos en forma de texto plano. A diferencia de otros formatos como JSON o XML, YAML es más fácil de leer y escribir para los humanos.

Veamos un ejemplo de cómo estructurar datos en YAML utilizando Bash:

```
# Estructura simple de datos en YAML
nombre: Juan
apellidos: Pérez
edad: 30
```

En el código anterior, utilizamos el formato clave: valor para definir los datos de una persona. Ahora, si queremos acceder a estos datos desde nuestro script de Bash, podemos hacerlo de la siguiente manera:

```
# Accediendo a los datos de YAML en Bash
nombre=$(cat datos.yml | grep 'nombre' | cut -d ':' -f 2)
apellidos=$(cat datos.yml | grep 'apellidos' | cut -d ':' -f 2)
edad=$(cat datos.yml | grep 'edad' | cut -d ':' -f 2)
echo "La persona es $nombre $apellidos, y tiene $edad años."
```

En este ejemplo, utilizamos el comando "grep" para buscar y filtrar los datos que necesitamos, y luego usamos "cut" para obtener solo el valor deseado. Esto nos permite acceder a los datos de YAML desde Bash de una manera clara y sencilla.

## Profundizando

Aparte de su facilidad de uso y legibilidad, YAML también tiene una serie de características que lo hacen aún más útil para programadores. Por ejemplo, es posible definir estructuras de datos anidadas y utilizar listas y diccionarios para organizar la información de una manera más compleja.

Además, YAML permite comentarios y etiquetas, lo que puede ser útil para documentar y facilitar la lectura del código. Y si necesitas compartir tus datos con otros sistemas no basados en YAML, ¡puedes convertir los datos de forma sencilla a otros formatos como JSON o XML!

## Ver también

- [Documentación oficial de YAML](https://yaml.org/)
- [Tutorial de YAML para programadores de Bash](https://medium.com/devtter/yaml-yaml-aint-markup-language-d002af47b9da)
- [Ejemplos de uso de YAML en Bash](https://github.com/jeffcsauer/bash-yaml-examples)