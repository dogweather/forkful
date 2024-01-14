---
title:    "Elm: Convirtiendo una fecha en una cadena"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Al programar en Elm, es común necesitar convertir fechas en formato de cadena (string) para mostrarlas en la interfaz de usuario. Esto puede resultar en un proceso confuso si no se tiene experiencia en programación funcional. En este artículo, te explicaremos por qué es necesario convertir una fecha en una cadena y cómo hacerlo de manera efectiva en Elm.

## Cómo

```elm
fecha = Date.fromCalendarDate 2020 4 15
cadena = Date.toString "dd/MM/yyyy" fecha
```

En este ejemplo, primero creamos una fecha utilizando la función `fromCalendarDate` y le asignamos los valores del año, mes y día que queremos. Luego, utilizamos la función `toString` para convertir la fecha en una cadena utilizando un formato específico, en este caso, "dd/MM/yyyy".

El resultado de `cadena` sería "15/04/2020". Es importante tener en cuenta que el formato utilizado dentro de la función `toString` debe estar en comillas dobles (""). Además, puedes cambiar el orden de los elementos en el formato para obtener diferentes resultados, como "MM/dd/yyyy" o "yyyy/MM/dd".

## Deep Dive

La razón por la cual es necesario convertir fechas en cadenas en Elm es porque el lenguaje trabaja principalmente con tipos de datos inmutables, lo que significa que no puedes modificar una fecha directamente. Por lo tanto, es necesario utilizar funciones como `toString` para obtener una representación legible de la fecha en lugar de tratar de modificarla directamente.

Además, Elm se basa en el tipo de dato `Date` para trabajar con fechas, el cual es un tipo bastante complejo que maneja una gran cantidad de información relacionada con la fecha y la hora. Al convertir una fecha en una cadena, puedes elegir qué información deseas mostrar de manera más sencilla.

## Ver también

- Documentación oficial de Elm sobre la conversión de fechas a cadenas: [https://package.elm-lang.org/packages/elm/time/latest/Date#toString](https://package.elm-lang.org/packages/elm/time/latest/Date#toString)
- Ejemplo de uso de `toString` en un proyecto de Elm: [https://github.com/elm/projects/tree/master/elm-datetime-formatter](https://github.com/elm/projects/tree/master/elm-datetime-formatter)