---
title:                "Obteniendo la fecha actual"
html_title:           "Ruby: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Obtener la fecha actual es una tarea común en la programación. Permite a los desarrolladores trabajar con el tiempo en sus programas, ya sea para mostrar la fecha a los usuarios, realizar cálculos de tiempo o establecer fechas de caducidad.

## Cómo hacerlo:
```Ruby
Time.now
```
Esta es la forma más sencilla de obtener la fecha y hora actuales en Ruby. Esto devolverá un objeto `Time` que contiene información como el año, mes, día, hora, minuto y segundo actuales.

Para obtener información más específica, como el día de la semana, la hora en un formato determinado o la hora en otra zona horaria, se puede utilizar el método `strftime`.

```Ruby
Time.now.strftime("Hoy es %A")
```
Salida: Hoy es jueves

```Ruby
Time.now.strftime("Son las %I:%M%p")
```
Salida: Son las 06:34PM

```Ruby
Time.now.getlocal("+02:00")
```
Con este método, se puede obtener la hora en una zona horaria específica. En el ejemplo, se devuelve la hora actual en el huso horario GMT +2.

## Profundizando:
Aunque `Time.now` es la forma más común de obtener la fecha y hora actuales en Ruby, también existen otras opciones. Por ejemplo, se pueden utilizar las gemas `Date` y `DateTime` para trabajar específicamente con fechas y tiempos respectivamente.

También es importante tener en cuenta que `Time.now` devuelve la hora del sistema en el que se está ejecutando el programa. Si se desea obtener la hora actual en una zona horaria diferente o en un servidor de tiempo en línea, se pueden utilizar otras opciones como la gema `time_difference`.

## Ver también:
Para obtener más información sobre cómo trabajar con fechas y tiempos en Ruby, se pueden consultar las siguientes fuentes:

- La documentación oficial de Ruby sobre la clase `Time`: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
- La gema `time_difference`: [https://github.com/tmlee/time_difference](https://github.com/tmlee/time_difference)
- El libro "The Ruby Way" de Hal Fulton: [https://www.amazon.com/gp/product/0672320835](https://www.amazon.com/gp/product/0672320835)