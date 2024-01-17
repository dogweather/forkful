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

## ¿Qué y por qué?
Comparar dos fechas es una tarea común en la programación, especialmente cuando se trabaja con datos relacionados al tiempo. Los programadores lo hacen para determinar la diferencia entre dos acontecimientos, como por ejemplo, calcular la edad de una persona o verificar si un plazo ha expirado.

## ¿Cómo hacerlo?
En Elixir, se puede comparar dos fechas utilizando el operador ">", lo que devuelve un valor booleano indicando si la primera fecha es posterior a la segunda. Por ejemplo: 
```
Elixir iex> {2021, 6, 1} > {2021, 5, 30}
true
```

También se puede utilizar la función `Date.compare/2` para comparar dos fechas y obtener un valor numérico que indique si la primera fecha es anterior, igual o posterior a la segunda. Por ejemplo:
```
Elixir iex> Date.compare({2021, 5, 1}, {2020, 5, 1})
1
```
En este caso, 1 indica que la primera fecha es posterior a la segunda.

## Un poco más profundo
La comparación de fechas ha sido un desafío para los programadores durante mucho tiempo, especialmente cuando se trata de variedades de formatos y calendarios. En lugar de comparar directamente las fechas, se pueden utilizar otras técnicas, como convertir las fechas a un formato conocido o utilizar librerías que manejen diferentes calendarios.

## Ver también
Para obtener más información sobre cómo trabajar con fechas, puedes verificar la documentación oficial de Elixir o consultar otras fuentes como [este artículo](https://blog.plataformatec.com.br/2014/03/working-with-dates-in-elixir/) que proporciona ejemplos prácticos de comparación de fechas en Elixir.