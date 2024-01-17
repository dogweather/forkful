---
title:                "Trabajando con csv"
html_title:           "Elixir: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

¡Hola a todos! ¿Están listos para aprender sobre cómo trabajar con CSV en Elixir? En este artículo, vamos a explorar qué es CSV y por qué los programadores lo utilizan. También veremos cómo podemos trabajar con CSV en Elixir con algunos ejemplos de código y finalmente, profundizar en algunos detalles más sobre esta técnica. ¡Así que empecemos!

## ¿Qué y Por Qué?

CSV significa "Comma Separated Values" (Valores Separados por Comas) en español. Es un formato de archivo comúnmente utilizado para almacenar datos en forma de una tabla simple con columnas y filas. Los programadores a menudo trabajan con CSV para procesar grandes cantidades de datos que pueden ser leídos fácilmente por programas.

## ¿Cómo?

Para trabajar con CSV en Elixir, utilizamos el módulo "CSV" que viene incluido en la biblioteca estándar de Elixir. Primero, tenemos que importar el módulo en nuestro archivo de código usando la directiva de importación ```import CSV```. Ahora, podemos utilizar la función ```CSV.parse``` para analizar un archivo CSV y obtener una lista de listas que representa los datos en el archivo.

Por ejemplo, si tenemos un archivo CSV llamado "datos.csv" con el siguiente contenido:

```
nombre,edad,país
Pablo,34,Argentina
Maria,28,España
```

Podemos cargar y analizar el archivo con el siguiente código:

```
import CSV

datos = CSV.parse(File.read!("datos.csv"), headers: true)
```

Después de esta operación, la variable datos contendrá una lista de listas donde cada elemento representa una fila del archivo CSV y cada valor separado por una coma se convierte en un elemento de la lista. También hemos utilizado el parámetro "headers: true" para indicar que la primera fila del archivo contiene los nombres de las columnas.

También podemos modificar y escribir un archivo CSV usando las funciones ```CSV.encode``` y ```File.write!```.

## Profundizando

El formato CSV fue inventado en los años 70 y originalmente fue utilizado en hojas de cálculo. Sin embargo, con el auge de la informática, se volvió ampliamente utilizado en la transferencia de datos entre diferentes programas. Aunque es un formato muy simple, es ampliamente compatible y fácil de manejar en casi cualquier lenguaje de programación.

Una alternativa popular al formato CSV es JSON, que es más estructurado y puede almacenar datos de forma más compleja. Sin embargo, CSV sigue siendo popular debido a su sencillez y facilidad de uso.

Para trabajar con CSV en Elixir, el módulo CSV utiliza una combinación de los módulos File y Stream para leer y escribir en archivos CSV. También ofrece muchas opciones para manejar diferentes formatos de entrada y salida, como tener una fila de encabezado o delimitadores de campo personalizados.

## Ver También

Si quieres profundizar más en el tema, te recomiendo revisar la documentación oficial de Elixir sobre el módulo CSV: [https://hexdocs.pm/elixir/CSV.html](https://hexdocs.pm/elixir/CSV.html).

¡Eso es todo por ahora! Espero que este artículo haya sido útil para entender cómo trabajar con CSV en Elixir. ¡Hasta la próxima!