---
title:                "Convertir una fecha en una cadena."
html_title:           "TypeScript: Convertir una fecha en una cadena."
simple_title:         "Convertir una fecha en una cadena."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es un proceso común en la programación. Esto significa tomar una fecha, que es una forma de almacenar información de tiempo, y convertirla en una cadena de caracteres legible para los usuarios. Los programadores realizan esta conversión para mostrar fechas en un formato específico o para manipularlas de alguna manera en su código.

## ¿Cómo hacerlo?

```TypeScript
let today = new Date(); // Crear una nueva fecha actual
let dateString = today.toDateString(); // Convertir en cadena de texto
console.log(dateString); // Muestra "miércoles, 29 de septiembre de 2021"
```

```TypeScript
let birthday = new Date(1990, 2, 20); // Crear una nueva fecha con año, mes y día
let formattedDate = birthday.toLocaleDateString("es-ES"); // Convertir en formato de fecha localizado en español
console.log(formattedDate); // Muestra "20/03/1990"
```

## Profundizando

Conversión de fecha en cadena de texto es una tarea común en la programación porque permite a los desarrolladores controlar cómo se muestran las fechas en sus aplicaciones. Además de utilizar el método `toLocaleDateString()` como en el ejemplo anterior, existen otras alternativas como `toUTCString()` para mostrar fechas en UTC o `toLocaleString()` para personalizar el formato de fecha y hora a través de opciones.

La conversión de una fecha en cadena de texto implica transformar la información de tiempo almacenada en la fecha en una representación legible para los usuarios. Esto generalmente implica identificar diferentes componentes de la fecha, como el año, mes y día, y combinarlos en un formato deseado. En la programación, se utilizan bibliotecas o funciones personalizadas para realizar esta tarea.

## Ver también

- [Documentación sobre el objeto Date en TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-5.html#date-object)
- [Guía de formatos de fecha y hora en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toLocaleDateString)