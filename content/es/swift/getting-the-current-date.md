---
title:    "Swift: Obteniendo la fecha actual"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Swift?

Obtener la fecha actual es una tarea común en la programación. Puede ser útil para registrar la fecha en que se realizó una acción, para mostrar la fecha actual en una aplicación o para realizar cálculos basados en la fecha. En Swift, obtener la fecha actual es una tarea sencilla que se puede realizar de varias maneras. A continuación, explicaremos cómo hacerlo.

## Cómo obtener la fecha actual en Swift

Existen diferentes maneras de obtener la fecha actual en Swift, y la elección dependerá del formato en que necesites la fecha y la hora. A continuación, veremos algunos ejemplos de código y su correspondiente salida:

```Swift
// Obtener la fecha y hora actual en formato completo
let fechaActual = Date()
print(fechaActual)

// Output: 2021-03-24 14:30:00 +0000
```

```Swift
// Obtener la fecha actual en formato día, mes y año
let fechaActual = Date()
let formato = DateFormatter()
formato.dateFormat = "dd/MM/yyyy"
let fechaConFormato = formato.string(from: fechaActual)
print(fechaConFormato)

// Output: 24/03/2021
```

```Swift
// Obtener la fecha actual en formato hora y minutos
let fechaActual = Date()
let formato = DateFormatter()
formato.dateFormat = "HH:mm"
let horaConFormato = formato.string(from: fechaActual)
print(horaConFormato)

// Output: 14:30
```

## Un vistazo más profundo

En Swift, la fecha y hora se manejan mediante el tipo de dato `Date`, que representa un momento en particular en el tiempo. Para trabajar con fechas, se pueden utilizar las clases `Date` y `DateFormatter`, que nos permiten obtener la fecha actual y darle un formato determinado.

Además, es importante tener en cuenta que la fecha y la hora pueden variar según la zona horaria en la que se encuentra el usuario. Por ello, es recomendable incluir en la aplicación una forma de ajustar la fecha y hora según la zona horaria del dispositivo del usuario.

## Ver también

- [Documentación oficial de Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Tutorial: Trabajar con fechas en Swift](https://www.appcoda.com/es/ejemplos-swift-trabajar-con-fechas/)

¡Esperamos que este artículo te haya sido de ayuda para entender cómo obtener la fecha actual en Swift! Ahora podrás implementar fácilmente esta funcionalidad en tus proyectos. ¡Hasta la próxima!