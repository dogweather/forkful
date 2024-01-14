---
title:    "Gleam: Extrayendo subcadenas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad esencial para cualquier programador, ya que permite manipular y trabajar con cadenas de texto de manera más eficiente. Con Gleam, esta tarea se vuelve aún más sencilla gracias a su sintaxis intuitiva y herramientas específicamente diseñadas para trabajar con cadenas.

## Cómo hacerlo

Para extraer una subcadena en Gleam, utilizamos la función `slice()` que toma como argumentos la cadena original, la posición inicial y la posición final de la subcadena que queremos extraer. Por ejemplo, si queremos extraer los caracteres de la posición 2 a la 7 de la cadena "Gleam es increíble", escribimos lo siguiente:

```Gleam
slice("Gleam es increíble", 2, 7)
```

El resultado sería la subcadena "eam es".

Además, con Gleam también podemos utilizar patrones y expresiones regulares para extraer subcadenas específicas de una cadena. Por ejemplo, si queremos extraer todas las palabras de una cadena que empiecen con la letra "h", podemos utilizar el patrón `r"h\w+"`, lo que nos devolvería una lista con todas las palabras que cumplan con ese patrón.

## Profundizando

Existen varios métodos y funciones en Gleam que nos permiten trabajar con subcadenas de manera aún más detallada. Por ejemplo, la función `contains()` nos permite verificar si una cadena contiene una subcadena específica, mientras que `split()` nos permite dividir una cadena en subcadenas más pequeñas utilizando un delimitador.

Otra funcionalidad interesante es la posibilidad de utilizar índices negativos en la función `slice()`, lo que nos permite extraer subcadenas desde el final de la cadena hacia el principio.

## Ver también

- Documentación oficial de Gleam para trabajar con cadenas: https://gleam.run/book/stdlib.html#string-manipulation
- Ejemplos de código para extraer subcadenas en Gleam: https://github.com/gleam-lang/gleam/blob/master/.workspace/source/examples/Slice.gleam
- Tutorial en español sobre cómo trabajar con cadenas en Gleam: https://dev.to/suricatala/como-trabajar-con-cadenas-de-texto-en-gleam-1dgo