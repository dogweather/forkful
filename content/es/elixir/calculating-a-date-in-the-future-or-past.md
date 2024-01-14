---
title:    "Elixir: Calculando una fecha en el futuro o pasado"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Por qué

A veces, necesitamos calcular una fecha en el pasado o en el futuro para nuestro programa. Por ejemplo, tal vez queramos mostrar una fecha límite para una tarea o una fecha en la que un evento tendrá lugar. En estos casos, es útil tener una forma de calcular fácilmente una fecha específica a partir de la fecha actual.

# Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Elixir, podemos utilizar la función `Date.add/2`. Esta función toma dos argumentos: una fecha y un número entero que representa la cantidad de días que queremos agregar a la fecha.

Por ejemplo, si queremos calcular una fecha 7 días después de la fecha actual, podemos hacerlo de la siguiente manera:

```Elixir
Date.add(Date.utc_today(), 7)
```

Esto devolverá una fecha en formato `{:ok, date}`, donde `date` será la fecha calculada. Del mismo modo, si queremos calcular una fecha 30 días antes de la fecha actual, podemos hacerlo así:

```Elixir
Date.add(Date.utc_today(), -30)
```

También podemos usar esta función con fechas específicas, por ejemplo:

```Elixir
my_date = Date.new(2021, 05, 01)
Date.add(my_date, -14)
```

Estos ejemplos devolverán fechas en formato `{:ok, date}` también. Pero si queremos ver la fecha en un formato más legible, podemos usar la función `Date.to_string/2`, que toma una fecha y un formato como argumentos y devuelve una cadena con la fecha formateada.

Por ejemplo, si queremos mostrar la fecha en formato "día-mes-año", podemos hacerlo así:

```Elixir
my_date = Date.new(2021, 05, 01)
Date.to_string(my_date, "{D}-{0M}-{YY}")
```

Esto devolverá una cadena con la fecha en el formato deseado: "1-05-21".

# Profundizando

Además de la función `Date.add/2`, también hay otras funciones útiles para trabajar con fechas en Elixir. Por ejemplo, la función `Date.add_years/3` permite agregar o restar años a una fecha, y la función `Date.diff/2` devuelve la diferencia en días entre dos fechas.

También es importante tener en cuenta que en Elixir, las fechas son inmutables, lo que significa que cada vez que realizamos una operación con ellas, se crea una nueva fecha en lugar de modificar la original.

# Ver también

- Documentación oficial de Elixir para el módulo Date: https://hexdocs.pm/elixir/Date.html
- Ejemplos de cálculos de fechas en Elixir: https://www.jesse-anderson.com/2018/05/working-with-dates-and-times-in-elixir/