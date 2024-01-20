---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una fecha en una cadena consiste en transformar un objeto de fecha a su representación textual. Los programadores hacen esto porque facilita la visualización y el procesamiento de fechas.

## Cómo hacerlo:

Elixir, mediante la función `Date.to_string/1` de su módulo `Date`, permite convertir una fecha en una cadena de texto. Mira este ejemplo:

```elixir
date = ~D[2022-08-24]
IO.puts Date.to_string(date)
```

El comando anterior imprimirá en consola:

```
"2022-08-24"
```

## Un vistazo más profundo

En Elixir, el módulo Date se basa en el calendario ISO. Esto implica que respeta tanto las reglas de las zonas horarias como los cambios en el tiempo a lo largo de la historia.

Existen otras formas de representar una fecha como cadena en Elixir. Por ejemplo, puedes utilizar `NaiveDateTime.to_string/1` o `DateTime.to_string/1` si necesitas más detalle acerca del tiempo.

Sobre la implementación, la función `Date.to_string/1` convierte la fecha en su representación textual acorde al formato "YYYY-MM-DD". Nota que siempre rellena el mes y el día con ceros a la izquierda si estos son números de un solo dígito.

## Ver También

Para más información y ejemplos acerca de cómo trabajar con fechas y tiempos en Elixir, visita los siguientes enlaces:

- [Documentación oficial de Elixir - Date](https://hexdocs.pm/elixir/Date.html)