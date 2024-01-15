---
title:                "Obteniendo la fecha actual."
html_title:           "Ruby: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Ruby?

Obtener la fecha actual en Ruby es una tarea común al desarrollar aplicaciones. Puede ser útil para mostrar la fecha a los usuarios, realizar operaciones basadas en la fecha actual o para registrar la fecha de creación o modificación de un archivo.

## Cómo hacerlo

Para obtener la fecha actual en Ruby, podemos utilizar el método `Date.today` de la clase `Date` de la librería standard. Este método devuelve una instancia de la clase `Date` que representar la fecha actual. Veamos un ejemplo de código:

```Ruby
hoy = Date.today
puts hoy
```

Este código imprimirá en la consola la fecha actual en formato "YYYY-MM-DD". También podemos utilizar los métodos `day`, `month` y `year` de la instancia `hoy` para obtener cada parte de la fecha por separado:

```Ruby
dia = hoy.day
mes = hoy.month
anio = hoy.year
puts "Hoy es #{dia} de #{mes} del #{anio}"
```

La librería standard también nos proporciona la clase `Time` para manejar fechas y horas. Utilizando el método `Time.now` podemos obtener la fecha y hora actuales:

```Ruby
ahora = Time.now
puts ahora
```

Al imprimir la variable `ahora` obtendremos un resultado similar a "YYYY-MM-DD HH:MM:SS +0000", donde HH representa la hora, MM los minutos y SS los segundos. También podemos utilizar los métodos `hour`, `min` y `sec` de la instancia `ahora` para obtener cada parte por separado.

## Profundizando

Si queremos obtener la fecha actual en otro formato, podemos utilizar el método `strftime` de la clase `Time`. Este método acepta una cadena de formato y devuelve una cadena con la fecha y hora en el formato deseado. Por ejemplo:

```Ruby
fecha = Time.now
puts fecha.strftime("%d/%m/%Y")
puts fecha.strftime("%A, %d de %B de %Y")
```

Esto imprimirá la fecha actual en los formatos "DD/MM/YYYY" y "Día de la semana, día de Mes de Año", respectivamente.

Además, podemos utilizar varios métodos de comparación y operaciones matemáticas con las instancias de `Time` y `Date` para realizar tareas como sumar o restar días a una fecha determinada.

## Ver también

- [Documentación de la librería standard de Ruby](https://ruby-doc.org/stdlib-2.7.0/)
- [Tutorial de Ruby en español](https://www.ruby-lang.org/es/documentation/quickstart/)