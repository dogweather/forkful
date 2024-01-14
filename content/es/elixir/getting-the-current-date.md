---
title:    "Elixir: Obteniendo la fecha actual"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Hay varias razones por las que alguien podría querer obtener la fecha actual en un programa de Elixir. Para algunos, puede ser necesario para fines de registro o seguimiento de tiempo, mientras que para otros puede ser útil para mostrar la fecha en una interfaz de usuario. Sea cual sea el motivo, en este artículo aprenderás cómo obtener la fecha actual en tu código de Elixir.

## Cómo

Obtener la fecha actual en Elixir es sorprendentemente simple gracias al módulo `DateTime`. Para comenzar, primero debemos importar el módulo y luego usar la función `utc_now/0` para obtener la fecha y hora actual en formato UTC.

```Elixir
import DateTime
current_date = utc_now()
```

Si queremos obtener la fecha y hora local, podemos usar la función `local_now/0` en su lugar. Además, podemos especificar el formato en el que queremos que se muestre la fecha utilizando el módulo `Calendar`.

```Elixir
import DateTime
import Calendar

current_date = utc_now()
formatted_date = Calendar.format!("{}/{}/{}", current_date.day, current_date.month, current_date.year)
```

Esto imprimiría la fecha en formato `DD/MM/AAAA`. También podemos obtener valores específicos de la fecha, como el día, el mes y el año, utilizando las funciones `day/1`, `month/1` y `year/1` respectivamente. Por ejemplo:

```Elixir
import DateTime

current_date = utc_now()
day = day(current_date)
month = month(current_date)
year = year(current_date)

IO.puts("Hoy es el día #{day} del mes #{month} del año #{year}")
```

## Profundizando

Ahora que sabemos cómo obtener la fecha actual en Elixir, es importante entender cómo se representa internamente en el lenguaje. En realidad, una fecha es sólo un `DateTime` en Elixir, que es una estructura compuesta de varios campos como año, mes, día, hora, minuto, etc. Estos campos se pueden acceder utilizando las funciones mencionadas anteriormente.

Además, el módulo `DateTime` tiene funciones para realizar operaciones cómo sumar o restar días, meses o años a una fecha. También es posible comparar fechas y obtener la diferencia entre ellas.

## Ver también

- [Documentación del módulo DateTime en Elixir](https://hexdocs.pm/elixir/DateTime.html)
- [Tutorial de Elixir para principiantes](https://www.freecodecamp.org/news/a-gentle-introduction-to-elixir-for-rubyists/) 
- [Ejemplos de código de Elixir](https://github.com/elixir-lang/elixir)