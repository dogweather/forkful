---
title:    "Gleam: Convirtiendo una cadena a minúsculas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos convertir una cadena de texto a minúsculas para realizar comparaciones o para mejorar la legibilidad del código. En Gleam, esto se puede hacer fácilmente con una operación integrada.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Gleam, simplemente utilizamos la función `String.to_lower()` y pasamos la cadena como argumento. Veamos un ejemplo:

```
Gleam
fn main() {
  let cadena = "HOLA A TODOS";
  let minuscula = String.to_lower(cadena);
  IO.println(minuscula);
}
```

La salida de este código será `hola a todos`, ya que la función `String.to_lower()` convierte todas las letras de la cadena a minúsculas.

## Profundizando

Es importante tener en cuenta que la operación `String.to_lower()` solo funciona con caracteres ASCII. Si utilizamos caracteres no ASCII, como letras con acentos o diéresis, no se convertirán a minúsculas.

Además, si queremos trabajar con cadenas que contengan caracteres Unicode, debemos usar la función `String.to_lowercase()` en su lugar. Esta función utiliza reglas de idioma para convertir los caracteres a minúsculas adecuados en lugar de simplemente cambiar la posición de las letras en el alfabeto.

## Ver también

- [Documentación oficial de Gleam sobre cadenas](https://gleam.run/documentation/guide/strings.html)
- [Ejemplos de código en Gleam](https://github.com/gleam-lang/examples/tree/master/basics)
- [Tutorial de programación en Gleam](https://www.youtube.com/playlist?list=PLE7oElOykoyceWx-Cnpej1VEwtv-hhHl2)