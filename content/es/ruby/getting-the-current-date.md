---
title:    "Ruby: Obteniendo la fecha actual"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Por Qué

¿Alguna vez te has preguntado por qué necesitamos saber la fecha y la hora exactas en un programa? Bueno, hay muchas razones para ello. Puede ser útil para programar eventos en nuestro calendario, para mostrar la fecha en una aplicación o simplemente para fines de registro. En cualquier caso, obtener la fecha y la hora actuales es una tarea muy común en la programación y hoy te enseñaré cómo hacerlo en Ruby.

# Cómo Hacerlo

En Ruby, hay una clase llamada `Time` que nos permite trabajar con fechas y horarios. Podemos crear una instancia de `Time` que represente la fecha y hora actuales usando el método `Now`. Veamos un ejemplo:

```Ruby
hoy = Time.now

puts hoy
```

La salida de este código será algo como esto:

```
2020-01-01 10:00:00 +0000
```

Como puedes ver, nos devuelve la fecha y hora actuales en formato de 24 horas. Podemos especificar el formato que deseamos utilizando el método `strftime`. Por ejemplo, si queremos que se muestre en formato de 12 horas, podemos usar lo siguiente:

```Ruby
puts hoy.strftime("%I:%M:%S %p")
```

Esto nos dará una salida como esta:

```
10:00:00 AM
```

Puedes encontrar una lista completa de formatos disponibles en la documentación de Ruby.

# Deep Dive

Si quieres profundizar un poco más en el tema de la fecha y hora en Ruby, aquí hay algunos conceptos interesantes que puedes explorar:

- La diferencia entre los métodos `now` y `today` en la clase `Time`.
- Cómo utilizar la clase `Date` para trabajar solo con fechas y no con horas.
- El uso del método `parse` para convertir una cadena en un objeto `Time`.

# Ver También

- [Documentación de Ruby sobre la clase `Time`](https://ruby-doc.org/core-2.7.1/Time.html)
- [Lista completa de formatos disponibles para el método `strftime`](https://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime)
- [Uso avanzado de la clase `Time` en Ruby](https://www.toptal.com/ruby/ruby-time-class)