---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comparar dos fechas es verificar cual viene después o si ambas son la misma. Los programadores hacen esto para tomar decisiones basadas en datos temporales.

## Cómo:

```Ruby
require 'date'

fecha1 = Date.new(2021, 11, 18)
fecha2 = Date.new(2022, 12, 24)

# Comparar fechas
if fecha1 > fecha2
   puts "La fecha1 #{fecha1} es mayor que la fecha2 #{fecha2}"
elsif fecha1 < fecha2
   puts "La fecha1 #{fecha1} es menor que la fecha2 #{fecha2}"
else
   puts "Las fechas son iguales"
end
```
Salida del código:

```Ruby
La fecha1 2021-11-18 es menor que la fecha2 2022-12-24
```

## Información Adicional:

1. **Contexto Histórico**: Ruby, lanzado en 1995, proporciona varias formas de comparar fechas debido a su sólido soporte de clase para la fecha en la biblioteca estándar. Esta funcionalidad ha estado disponible desde las primeras versiones.

2. **Alternativas**: Otra opción para comparar fechas es convertirlas a Time y usar la comparación de épocas (seconds since the Unix epoch).

```Ruby
fecha1 = Time.new(2021, 11, 18).to_i
fecha2 = Time.new(2022, 12, 24).to_i

if fecha1 > fecha2
   puts 'fecha1 es mayor que fecha2'
elsif fecha1 < fecha2
   puts 'fecha1 es menor que fecha2'
else
   puts 'Las fechas son iguales'
end
```
3. **Detalles de la Implementación**: Bajo el capó, la clase de Fecha utiliza Spaceship Operator (`<=>`) para la comparación, que devuelve -1, 0, o 1 dependiendo si la fecha a compare es menor, igual, o mayor.

## Ver También:
1. Documentación oficial Ruby - Clase Fecha: https://docs.ruby-lang.org/en/3.0.0/Date.html
2. Stack Overflow - Cómo comparar fechas en Ruby: https://stackoverflow.com/questions/2670363/how-to-compare-dates-in-ruby