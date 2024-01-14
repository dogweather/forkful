---
title:    "Ruby: Obteniendo la fecha actual"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por qué obtener la fecha actual en Ruby

¿Se ha preguntado alguna vez cómo obtener la fecha actual en Ruby? Puede parecer un concepto sencillo, pero en realidad hay diferentes formas de hacerlo y pueden surgir dudas al momento de implementarlo. En esta publicación, exploraremos por qué es útil obtener la fecha actual en Ruby, cómo hacerlo y profundizamos en algunos detalles adicionales.

## Cómo obtener la fecha actual en Ruby

Hay varias formas de obtener la fecha actual en Ruby, dependiendo de lo que estés buscando exactamente. Una forma sencilla es utilizar el método `Time.now`, que devuelve la fecha y hora actuales en formato de objeto en Ruby.

```Ruby
Time.now
# => 2021-08-02 15:30:00 +0100
```

Otra opción es utilizar el método `Date.today`, que devuelve la fecha actual en formato de objeto `Date`.

```Ruby
Date.today
# => #<Date: 2021-08-02 ((2459415j,0s,0n),+0s,2299161j)>
```

También puedes utilizar el método `DateTime.now` para obtener tanto la fecha como la hora actual en formato de objeto `DateTime`.

```Ruby
DateTime.now
# => #<DateTime: 2021-08-02T15:30:00+01:00 ((2459415j,5100s,548283574n),+3600s,2299161j)>
```

## Inmersión en detalle

La clase `Time` en Ruby proporciona una cantidad significativa de métodos y opciones para trabajar con fechas y horas. Por ejemplo, puedes utilizar los métodos `Time#year`, `Time#month` y `Time#day` para obtener el año, mes y día específicos de una fecha. También puedes formatear la fecha de diferentes maneras utilizando el método `strftime`.

```Ruby
Time.now.year
# => 2021

Time.now.month
# => 8

Time.now.day
# => 2

Time.now.strftime("%d/%m/%Y")
# => 02/08/2021
```

Además, si necesitas trabajar con la zona horaria, puedes utilizar el método `in_time_zone` en objetos tipo `Time` y `DateTime`.

```Ruby
Time.now
# => 2021-08-02 15:30:00 +0100

Time.now.in_time_zone("Eastern Time (US & Canada)")
# => 2021-08-02 10:30:00 -0400

DateTime.now
# => #<DateTime: 2021-08-02T15:30:00+01:00 ((2459415j,5100s,548283574n),+3600s,2299161j)>

DateTime.now.in_time_zone("Pacific Time (US & Canada)")
# => #<DateTime: 2021-08-02T07:30:00-07:00 ((2459415j,2100s,434283574n),-25200s,2299161j)>
```

## Ver también

- [Documentación de Ruby sobre la clase Time](https://ruby-doc.org/core/Time.html)
- [Documentación de Ruby sobre la clase Date](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
- [Documentación de Ruby sobre la clase DateTime](https://ruby-doc.org/stdlib/libdoc/date/rdoc/DateTime.html)

¡Y eso es todo! Esperamos que esta publicación te haya ayudado a comprender cómo obtener la fecha actual en Ruby y cómo trabajar con ella en tu código. ¡Feliz programación!