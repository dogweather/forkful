---
title:                "C#: Convirtiendo una fecha en un string"
simple_title:         "Convirtiendo una fecha en un string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Cuando estamos escribiendo una aplicación, a menudo nos encontramos con la necesidad de convertir una fecha en una cadena de caracteres. Esto puede ser útil para formatear una fecha para mostrarla en una interfaz de usuario, para almacenarla en una base de datos o para enviarla a un servicio web. En esta publicación, aprenderemos cómo convertir una fecha en una cadena en C#.

## Cómo hacerlo

¡Convertir una fecha en una cadena en C# es muy sencillo! Solo necesitamos utilizar el método `ToString` de la clase `DateTime`. Veamos un ejemplo de código:

```C#
DateTime fecha = new DateTime(2020, 10, 1);
string fechaComoString = fecha.ToString("dd/MM/yyyy");

Console.WriteLine(fechaComoString);
```

El resultado de este código será `01/10/2020`. En este ejemplo, creamos una instancia de la clase `DateTime` con la fecha 1 de octubre de 2020 y luego utilizamos el método `ToString` con el formato deseado para convertir la fecha en una cadena.

Pero ¿qué pasa si queremos incluir la hora en la cadena también? ¡No hay problema! Podemos usar el mismo método `ToString` con un formato diferente:

```C#
DateTime fecha = new DateTime(2020, 10, 1, 12, 30, 0);
string fechaConHora = fecha.ToString("dd/MM/yyyy hh:mm:ss");

Console.WriteLine(fechaConHora);
```

El resultado de este código será `01/10/2020 12:30:00`. En este caso, le damos al método un formato que incluye la hora, los minutos y los segundos. Hay muchos otros formatos disponibles, así que asegúrate de explorar y encontrar el que mejor se adapte a tus necesidades.

## Profundizando

El método `ToString` es muy poderoso y nos permite convertir una fecha en una cadena con el formato que deseamos. Pero ¿cómo funciona exactamente? Bueno, en realidad, este método no es exclusivo de la clase `DateTime`. De hecho, está presente en todas las clases que heredan de la clase `Object`. Esto significa que también podemos usarlo en nuestras propias clases personalizadas.

Además, podemos utilizar el método `ToString` con argumentos adicionales, como una instancia de la clase `IFormatProvider` o un formato específico de un idioma. Esto nos da aún más control sobre cómo se formatea nuestra cadena de fecha.

## Ver También

Puedes aprender más sobre el método `ToString` en la [documentación oficial de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.object.tostring?view=netcore-3.1) y practicar con diferentes formatos en [este sitio web de ejemplos](https://docs.microsoft.com/es-es/dotnet/standard/base-types/custom-date-and-time-format-strings). ¡Feliz codificación!