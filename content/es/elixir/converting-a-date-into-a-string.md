---
title:    "Elixir: Convirtiendo una fecha en un string"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Con frecuencia en la programación, es necesario convertir una fecha en una cadena de texto para poder mostrarla en una interfaz de usuario o almacenarla en una base de datos. En Elixir, podemos hacer esto de forma rápida y sencilla utilizando algunas funciones y módulos incorporados en el lenguaje.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Elixir, podemos utilizar la función `DateTime.to_string/2` y el módulo `Date`:

```
Elixir DateTime.to_string(%{year: 2021, month: 10, day: 18})
#=> "2021-10-18 00:00:00"

Elixir Date.to_string({2021, 10, 18})
#=> "2021-10-18"
```

También podemos especificar un formato personalizado utilizando la función `DateTime.to_string/3`, que toma como argumentos la fecha, el formato y la zona horaria:

```
Elixir DateTime.to_string(%{year: 2021, month: 10, day: 18}, "{M}/{D}/{YY}")
#=> "10/18/21"
```

El módulo `Date` también ofrece la función `Date.to_string!/2`, que devuelve una cadena de texto sin formato para fechas que no se pueden representar en formato ISO:

```
Elixir Date.to_string!({2021, 2, 30})
#=> "2021-02-30"
```

## Profundizando

Detrás de estas funciones y módulos se encuentran varios protocolos y tipos de datos en Elixir. Uno de ellos es el protocolo `DateTime.Format`, que define cómo se debe formatear un `DateTime` para su representación en una cadena.

Además, los tipos concretos de fechas en Elixir son `DateTime`, `Date`, y `NaiveDateTime` (que no almacena información de zona horaria), y todos implementan el protocolo `Calendar.ISO`. Por lo tanto, podemos utilizar las mismas funciones en todos ellos.

## Ver también

- [Documentación de Elixir sobre fechas y horarios](https://hexdocs.pm/elixir/Calendar.html)
- [Ejemplos de formato de fechas en Elixir](https://devhints.io/elixir-date-time)
- [Cheat sheet de Elixir sobre fechas y horarios](https://docs.plataformatec.com.br/elixir-cheatsheets/elixir-date-time.html)