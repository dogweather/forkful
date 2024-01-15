---
title:                "Comenzando un nuevo proyecto"
html_title:           "Gleam: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Por qué iniciar un nuevo proyecto?

Iniciar un nuevo proyecto en Gleam es una excelente opción para aquellos que buscan un lenguaje de programación funcional con un fuerte enfoque en la concurrencia y la seguridad del tipo de datos. Además, Gleam utiliza la plataforma de compilación Erlang, lo que lo hace ideal para aplicaciones escalables y tolerantes a fallos.

## Cómo hacerlo

```Gleam
// Declaración de una función
fn suma(a, b) {
  a + b
}

// Llamada a la función y salida
println(suma(5, 3))
// Salida: 8
```

Para iniciar un nuevo proyecto en Gleam, primero debemos instalar el compilador de Gleam y crear una nueva carpeta para nuestro proyecto. Luego, podemos utilizar el comando `gleam new` para crear una plantilla básica del proyecto. Podemos agregar nuestras funciones y estructuras de datos personalizadas al archivo `src/proyecto.gleam` y compilar nuestro código utilizando el comando `gleam build`. Una vez compilado, podemos ejecutar nuestro proyecto utilizando el comando `gleam run`.

## Profundizando en cómo iniciar un nuevo proyecto

Al iniciar un nuevo proyecto en Gleam, es importante tener en cuenta los conceptos básicos de la programación funcional y cómo difieren de los enfoques imperativos más comunes. También debemos considerar cómo Gleam utiliza la concurrencia para mejorar la eficiencia del código y cómo podemos aprovecharla en nuestro proyecto. Finalmente, no debemos olvidar explorar las poderosas estructuras de datos y funciones de alto orden que Gleam tiene para ofrecer.

## Ver también

- [Página oficial de Gleam](https://gleam.run/)
- [Documentación de Gleam](https://gleam.run/book/introduction.html)
- [Erlang OTP](https://www.erlang.org/docs)