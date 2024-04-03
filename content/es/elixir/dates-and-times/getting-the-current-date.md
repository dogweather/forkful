---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:25.678498-07:00
description: "Obtener la fecha actual en Elixir implica acceder a la informaci\xF3\
  n de fecha y hora del sistema, una tarea com\xFAn para registrar, marcar datos o\
  \ cualquier\u2026"
lastmod: '2024-03-13T22:44:58.712690-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en Elixir implica acceder a la informaci\xF3n de\
  \ fecha y hora del sistema, una tarea com\xFAn para registrar, marcar datos o cualquier\
  \ funcionalidad que requiera conocimiento de la fecha actual."
title: Obteniendo la fecha actual
weight: 29
---

## Qué y Por Qué?
Obtener la fecha actual en Elixir implica acceder a la información de fecha y hora del sistema, una tarea común para registrar, marcar datos o cualquier funcionalidad que requiera conocimiento de la fecha actual. Esta operación es esencial para crear aplicaciones conscientes del tiempo y para tareas como generar informes o timestamps en una aplicación web.

## Cómo hacerlo:
La biblioteca estándar de Elixir, a través del módulo `DateTime`, permite obtener la fecha y hora actuales. Dado que Elixir se ejecuta en la VM de Erlang (BEAM), aprovecha las funcionalidades subyacentes de Erlang para las operaciones de tiempo.

### Usando la Biblioteca Estándar de Elixir
Elixir proporciona la función `DateTime.utc_now/0` para obtener la fecha y hora actuales en UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Salida de Ejemplo:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Para obtener solo la fecha actual, se pueden extraer los componentes de año, mes y día:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Salida de Ejemplo:**
```
~D[2023-05-04]
```

### Utilizando la Biblioteca Timex
Para requisitos de fecha y hora más complejos, se puede utilizar una biblioteca de terceros popular llamada Timex. Primero, añade `Timex` a tus dependencias en mix.exs:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Después de instalar la dependencia (`mix deps.get`), puedes usar Timex para obtener la fecha actual:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Salida de Ejemplo:**
```
~D[2023-05-04]
```

Timex ofrece funcionalidades extensas para la manipulación de fecha-hora, convirtiéndolo en una poderosa adición a tus aplicaciones Elixir, especialmente cuando se trata de zonas horarias, formateo y análisis de fechas y horas.

Al entender y utilizar las capacidades integradas de Elixir y la biblioteca Timex, puedes trabajar fácilmente con fechas y horas en tus aplicaciones Elixir, adaptando la experiencia a las necesidades de tu aplicación con precisión y facilidad.
