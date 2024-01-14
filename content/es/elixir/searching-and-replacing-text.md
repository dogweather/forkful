---
title:    "Elixir: Buscando y reemplazando texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## ¿Por qué usar Elixir para buscar y reemplazar texto?

Buscar y reemplazar texto es una tarea común en el desarrollo de software. Con Elixir, puedes hacerlo de manera eficiente y elegante gracias a su sintaxis sencilla y su poderoso motor de expresiones regulares. ¡Descubre por qué tantos desarrolladores confían en Elixir para esta tarea!

## Cómo hacerlo en Elixir

Para buscar y reemplazar texto en Elixir, podemos usar la función `String.replace/4`. Simplemente pasamos la cadena original, el patrón de búsqueda, el texto de reemplazo y las opciones como argumentos. Por ejemplo:

```Elixir
original = "¡Hola mundo!"
pattern = "mundo"
replacement = "universo"
options = [:global]

String.replace(original, pattern, replacement, options)
```

Esto devuelve la cadena "¡Hola universo!", ya que hemos reemplazado todas las ocurrencias de "mundo" con "universo" en la frase original. Otras opciones disponibles incluyen `:case_insensitive` para ignorar mayúsculas y minúsculas, y `:unicode` para manejar caracteres unicode en el patrón y el texto de reemplazo.

Otro enfoque utilizando el módulo `Regex` es el uso de la función `Regex.replace/3`. Esta función requiere que primero creemos un patrón de expresión regular y luego lo pasemos junto con el texto original y el texto de reemplazo. Por ejemplo:

```Elixir
original = "¡Hola mundo!"
pattern = ~r/mundo/
replacement = "universo"

Regex.replace(pattern, original, replacement)
```

## Profundizando en la búsqueda y reemplazo de texto

Elixir también ofrece múltiples opciones y características para una búsqueda y reemplazo más avanzadas. Estas incluyen el uso de expresiones regulares en `String.replace/4`, la posibilidad de usar funciones de reemplazo personalizadas en `Regex.replace/3`, y la capacidad de hacer búsquedas y reemplazos en diferentes tipos de datos, como listas y mapas.

Además, Elixir proporciona herramientas para trabajar con cadenas en diferentes codificaciones, lo que permite una búsqueda y reemplazo precisa en cualquier tipo de texto.

## Ver también

- Documentación oficial de Elixir sobre `String.replace/4`: https://hexdocs.pm/elixir/String.html#replace/4
- Ejemplos de uso de `Regex` en la documentación de Elixir: https://hexdocs.pm/elixir/Regex.html#content
- Tutoriales y recursos sobre el uso de expresiones regulares en Elixir: https://dev.to/numbersandbetters/regular-expressions-in-elixir-scanner-and-regex-1fi1