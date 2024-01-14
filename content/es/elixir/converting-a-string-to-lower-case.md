---
title:    "Elixir: Convirtiendo una cadena a minúsculas"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena de texto a minúsculas?

Convertir una cadena de texto a minúsculas es una operación muy común en la programación. Puede ser útil para comparar cadenas, normalizar datos o simplemente para mejorar la legibilidad del texto. En Elixir, hay varias formas de realizar esta conversión, por lo que es una habilidad importante para cualquier programador de este lenguaje.

## ¿Cómo hacerlo?

Para convertir una cadena de texto a minúsculas en Elixir, podemos utilizar la función `String.downcase/1`. Esta función toma como argumento una cadena de texto y devuelve una nueva cadena con todas las letras en minúscula. Veamos un ejemplo:

```Elixir
cadena = "Hola amigo!"
nueva_cadena = String.downcase(cadena)
IO.puts nueva_cadena
```

Este código imprimirá "hola amigo!" en la consola. También podemos utilizar esta función para comparar cadenas de texto, ya que nos aseguramos de que todas las letras estén en el mismo formato.

Además de `String.downcase/1`, también podemos utilizar la función `String.downcase/2` para especificar un módulo que controle la conversión. Por ejemplo, si queremos utilizar las reglas de conversión del idioma español, podemos hacerlo de la siguiente manera:

```Elixir
cadena = "HOLA AMIGO!"
nueva_cadena = String.downcase(cadena, :es)
IO.puts nueva_cadena
```

Este código imprimirá "hola amigo!" en la consola, ya que `:es` es el módulo que controla la conversión de mayúsculas a minúsculas en español.

## Profundizando

En profundidad, la función `String.downcase/1` utiliza internamente la función `String.normalize/2`, que convierte la cadena de texto a su forma normalizada. Luego, elimina todos los caracteres que no sean letras y los convierte a minúsculas. Las letras con acentos también se convierten a sus equivalentes en minúsculas.

También es importante tener en cuenta que la conversión de mayúsculas a minúsculas no es lo mismo en todos los idiomas. Por ejemplo, en turco, algunas letras tienen dos formas diferentes de minúsculas dependiendo del contexto. Por eso, es recomendable utilizar la función `String.downcase/2` con el módulo correspondiente según sea necesario.

## Vea también

- Documentación oficial de Elixir sobre `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Documentación oficial de Elixir sobre `String.downcase/2`: https://hexdocs.pm/elixir/String.html#downcase/2