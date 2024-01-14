---
title:    "Ruby: Convirtiendo una fecha en una cadena"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué convertir una fecha a una cadena en Ruby

Muchas veces, al trabajar con fechas en Ruby, es necesario convertirlos en cadenas de texto para poder mostrarlos correctamente o realizar ciertas operaciones. Conocer cómo convertir una fecha a una cadena en Ruby puede ser muy útil en diversas situaciones.

## Cómo hacerlo

Para convertir una fecha a una cadena en Ruby, podemos utilizar el método `strftime`, que nos permite formatear una fecha a una cadena según un patrón especificado. Veamos un ejemplo:

```Ruby
date = Date.today
puts date.strftime("%d/%m/%Y")
```

En este caso, el método `strftime` toma dos argumentos: el primero es el patrón de formato, y el segundo es la fecha que queremos convertir. En el ejemplo, le estamos indicando que queremos que la fecha se muestre en formato día/mes/año. La salida de este código sería: `09/07/2021`.

## Profundizando en la conversión de fechas a cadenas

El patrón de formato que se utiliza en `strftime` es muy amplio, por lo que es recomendable consultar la documentación para conocer todas las opciones disponibles. Algunos de los más comunes son:

- `%d`: día del mes (2 dígitos)
- `%m`: mes del año (2 dígitos)
- `%Y`: año con 4 dígitos
- `%H`: hora en formato de 24 horas (2 dígitos)
- `%M`: minutos (2 dígitos)
- `%S`: segundos (2 dígitos)
- `%b`: mes abreviado (ej. Jul)
- `%B`: mes completo (ej. Julio)
- `%A`: día de la semana completo (ej. Viernes)
- `%a`: día de la semana abreviado (ej. Vie)
- `%w`: día de la semana en número (domingo = 0, lunes = 1, etc.)

Además de estos, existen muchos más patrones disponibles para dar formato a fechas y horas. Puedes experimentar con ellos y seguramente encontrarás el que más se ajuste a tus necesidades.

## Ver también

- [Documentación de Ruby sobre el método `strftime`](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)
- [Otras formas de formatear fechas en Ruby](https://www.rubyguides.com/2015/05/working-with-dates-and-time-in-ruby/)
- [Método `String#to_date` para convertir cadenas a fechas en Ruby](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html#method-i-align-3F)