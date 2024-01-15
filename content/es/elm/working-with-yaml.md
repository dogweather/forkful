---
title:                "Trabajando con yaml"
html_title:           "Elm: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por Qué

Si eres un desarrollador o estudiante de programación, es probable que hayas escuchado sobre YAML. Pero, ¿qué es exactamente y por qué deberías aprender a trabajar con él? Bueno, YAML es un lenguaje de formato de texto que se utiliza para crear archivos de configuración en tus proyectos de software. Te permite organizar tus datos de forma clara y fácil de entender, lo que lo hace muy útil para crear configuraciones complejas. Además, es fácil de aprender y de usar, lo que lo convierte en una herramienta imprescindible en el mundo de la programación.

## Cómo Hacerlo

Para trabajar con YAML en Elm, necesitarás una biblioteca llamada `zwilias/elm-yaml`. Primero, deberás agregarla a tus dependencias en tu archivo `elm.json`. Luego, podrás importarla en tus archivos de Elm.

```elm
import Yaml.Decode as Yaml
```

Ahora, puedes empezar a codificar tu archivo YAML. Comencemos con un simple ejemplo de una lista de nombres:

```elm
yaml =
  """
  - John
  - Sarah
  - Michael
  """
```

Para decodificar este archivo YAML, puedes usar la función `decodeString`, que tomará tu archivo YAML como una cadena y lo convertirá en un valor de Elm. En este caso, el valor resultante será una lista de cadenas.

```elm
decodedNames =
  Yaml.decodeString yaml
```

Ahora, si queremos imprimir estos nombres en la consola, podemos usar la función `Debug.log`:

```elm
Debug.log "Nombres:" decodedNames
```

El resultado en la consola sería algo como esto:

```
Nombres: ["John", "Sarah", "Michael"]
```

¡Genial! ¡Ahora tienes un buen comienzo para trabajar con YAML en Elm! Puedes seguir explorando la sintaxis y las funciones de decodificación de YAML para crear configuraciones más complejas.

## Profundizando

Además de la función `decodeString`, la biblioteca `zwilias/elm-yaml` ofrece otras funciones para decodificar diferentes tipos de datos, como cadenas, enteros, booleanos y más. También puedes decodificar tus propios tipos de datos personalizados utilizando la función `map`.

Otra característica interesante de esta biblioteca es que es compatible con el sistema de tipos de Elm, lo que significa que puedes asegurarte de que tu archivo YAML esté bien estructurado y evitar errores de decodificación.

Mientras sigas aprendiendo y experimentando con YAML en Elm, verás lo útil y poderoso que puede ser para crear configuraciones flexibles y fáciles de mantener en tus proyectos.

## Ver También

- Documentación de la biblioteca `zwilias/elm-yaml`: https://package.elm-lang.org/packages/zwilias/elm-yaml/latest/
- Ejemplos de código YAML en Elm: https://github.com/zwilias/elm-yaml/blob/master/examples/Basic.elm