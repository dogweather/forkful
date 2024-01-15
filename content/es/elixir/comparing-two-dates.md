---
title:                "Comparando dos fechas"
html_title:           "Elixir: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Elixir?

Comparar dos fechas puede ser útil en numerosas situaciones, como ordenar eventos en una aplicación o filtrar datos en una base de datos. Además, al ser Elixir un lenguaje funcional, su manera de manejar y comparar fechas es diferente a otros lenguajes, lo que puede ser interesante para aprender y explorar.

## ¿Cómo hacerlo en Elixir?

En Elixir, hay dos maneras de comparar fechas: utilizando operadores de comparación o utilizando funciones específicas para comparar. Veamos un ejemplo de cada una utilizando las fechas 1 de enero de 2020 y 1 de febrero de 2020:

```
iex> {2020, 1, 1} > {2020, 2, 1}
true
```

```
iex> Date.compare({2020, 1, 1}, {2020, 2, 1})
:lt
```

En el primer ejemplo, utilizamos el operador `>` para comparar las fechas. En el segundo ejemplo, utilizamos la función `compare` de la librería `Date` para hacer la comparación. Ambas formas son válidas y pueden ser utilizadas según la preferencia del programador.

## Profundizando en la comparación de fechas en Elixir

Elixir maneja internamente las fechas como una tupla con tres elementos: año, mes y día. Esto significa que la comparación de fechas se basa en el orden de sus elementos, es decir, primero se compara el año, después el mes y finalmente el día. Además, Elixir también proporciona funciones especiales para comparar fechas que tengan en cuenta el tiempo, como por ejemplo `DateTime.compare`.

En el caso de tener fechas con diferente formato, es posible convertirlas a formato `Date` utilizando la función `Date.from_iso8601`. También es importante tener en cuenta que la comparación de fechas en Elixir no es "inteligente", es decir, no se pueden comparar fechas con texto o con otros formatos que no sean el `DateTime` de Elixir.

## Ver también

- Documentación oficial de Elixir sobre fechas: https://hexdocs.pm/elixir/Date.html
- Tutorial sobre cómo trabajar con fechas en Elixir: https://www.pluralsight.com/guides/handling-dates-elixir