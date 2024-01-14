---
title:                "Kotlin: Comparando dos fechas"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué Comparar Dos Fechas

Si eres un desarrollador de Kotlin, es probable que en algún momento necesites comparar dos fechas en tu código. Esto puede ser útil para realizar tareas como verificar la validez de una contraseña o para determinar si un evento ya ha pasado. En esta entrada de blog, te mostraremos cómo puedes comparar dos fechas en Kotlin de manera efectiva.

## Cómo Comparar Dos Fechas

Para comparar dos fechas en Kotlin, utilizaremos el método `compareTo()` de la clase `Date` de Java. Primero, debemos crear dos objetos `Date` que contengan las fechas que queremos comparar. Por ejemplo:

```
Kotlin val fecha1 = Date(2020, 10, 05)
val fecha2 = Date(2020, 11, 10)
```

Luego, podemos utilizar el método `compareTo()` para comparar estas dos fechas y almacenar el resultado en una variable:

```
Kotlin val resultado = fecha1.compareTo(fecha2)
```

El método `compareTo()` devuelve un número entero que indica la relación entre las dos fechas. Este número puede ser:

- Menor que cero si la primera fecha es anterior a la segunda fecha
- Cero si ambas fechas son iguales
- Mayor que cero si la primera fecha es posterior a la segunda fecha

Podemos utilizar este resultado para realizar cualquier acción que necesitemos. Por ejemplo, podemos imprimir un mensaje dependiendo del resultado de la comparación:

```
Kotlin if (resultado < 0) {
    println("La primera fecha es anterior a la segunda fecha")
} else if (resultado == 0) {
    println("Ambas fechas son iguales")
} else {
    println("La primera fecha es posterior a la segunda fecha")
}
```

## Profundizando en la Comparación de Fechas

Es importante tener en cuenta que el método `compareTo()` de la clase `Date` compara las fechas teniendo en cuenta no solo el día, sino también la hora. Esto significa que si solo queremos comparar las fechas sin tener en cuenta la hora, debemos establecer la hora en ambas fechas en el mismo valor. Por ejemplo:

```
Kotlin val fecha1 = Date(2020, 10, 05, 0, 0, 0)
val fecha2 = Date(2020, 11, 10, 0, 0, 0)
```

También es importante tener en cuenta que el método `compareTo()` solo compara las fechas en su formato predeterminado, que es el horario GMT. Si estás trabajando con fechas en un huso horario diferente, es posible que obtengas resultados inesperados. En este caso, es recomendable utilizar la biblioteca de fechas y horarios de Kotlin, la cual ofrece funciones más avanzadas para trabajar con fechas.

## Ver también

- [Documentación oficial de Kotlin sobre la comparación de fechas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.util.-date/compare-to.html)
- [Biblioteca de fechas y horarios de Kotlin](https://github.com/Kotlin/kotlinx-datetime)