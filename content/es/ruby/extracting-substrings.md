---
title:                "Ruby: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en Ruby

Extraer subcadenas, o partes de una cadena de texto, puede parecer una tarea simple en el lenguaje de programación Ruby. Sin embargo, comprender cómo y por qué extraer subcadenas puede ser útil en tus programas de Ruby puede mejorar tu flujo de trabajo y ayudarte a escribir código más eficiente. En esta publicación de blog, exploraremos el "por qué" y "cómo" de la extracción de subcadenas en Ruby, así como algunas reflexiones más profundas sobre este tema.

## Cómo hacerlo

La forma más común de extraer subcadenas en Ruby es utilizando el método `slice` o su alias `[]`. Estos métodos toman dos argumentos: el índice inicial y el índice final de la subcadena que deseas extraer. Por ejemplo, si tenemos una cadena de texto "Hola mundo", podemos extraer la subcadena "mundo" usando `slice` de la siguiente manera:

```Ruby
"Hola mundo".slice(5, 4)
# Output: "mundo"
```

También puedes especificar un rango de índices para extraer una subcadena más grande, como en el siguiente ejemplo:

```Ruby
"Hola mundo".slice(2, 6)
# Output: "la mund"
```

Además, puedes utilizar números negativos para especificar índices a partir del final de la cadena. Por ejemplo, si queremos extraer la subcadena "Hola" de nuestro texto, podemos hacerlo de la siguiente manera:

```Ruby
"Hola mundo".slice(-10, 4)
# Output: "Hola"
```

## Profundizando

Ahora que sabes cómo extraer subcadenas usando `slice` en Ruby, es importante comprender cómo funcionan los índices que pasamos como argumentos. Por ejemplo, el primer argumento, el índice inicial, representa el primer carácter de la subcadena que deseas extraer. Si ese índice no existe en la cadena, el método devolverá `nil`. Además, si no se proporciona el segundo argumento, se utilizará el índice final por defecto, que será el último carácter de la cadena.

También es importante tener en cuenta que los índices en Ruby comienzan en 0, lo que significa que si tienes una cadena de texto de 10 caracteres, el carácter en el índice 9 será el último en la cadena.

## Ver también

Para obtener más información sobre la extracción de subcadenas en Ruby, consulta esta [guía](https://www.rubyguides.com/2019/01/ruby-substring/) o la [documentación oficial](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice) de Ruby.

Esperamos que esta publicación te haya ayudado a comprender mejor cómo y por qué extraer subcadenas en Ruby. ¡Prueba diferentes ejemplos y sácale el máximo provecho a este útil método en tus programas!