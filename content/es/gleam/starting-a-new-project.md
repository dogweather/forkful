---
title:                "Gleam: Comenzando un nuevo proyecto"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Por qué comenzar un nuevo proyecto en Gleam?

Comenzar un nuevo proyecto en Gleam puede ser una gran elección para aquellos que buscan un lenguaje de programación moderno y seguro. Con su tipado estático y su enfoque en la concurrencia, Gleam es una herramienta poderosa para crear aplicaciones robustas y escalables. Además, tiene una sintaxis elegante y fácil de aprender, lo que lo convierte en una excelente opción para desarrolladores de todos los niveles de experiencia.

## Cómo iniciar un proyecto en Gleam

Antes de comenzar a escribir código en Gleam, tendremos que instalar el compilador y configurar un entorno de desarrollo. Para hacerlo, podemos seguir estos pasos:

1. Instalar el compilador de Gleam utilizando el administrador de paquetes de nuestra elección.
2. Crear una estructura de carpetas para nuestro proyecto y navegar hasta ella.
3. Inicializar el proyecto con el comando `gleam new <nombre_del_proyecto>`.
4. Esto creará una estructura básica de archivos para nuestro proyecto, incluyendo un archivo `src/main.gleam`, que es donde escribiremos nuestro código.

Ahora que tenemos nuestro proyecto configurado, podemos comenzar a escribir código en Gleam. Aquí hay un ejemplo simple de un programa que imprime "¡Hola mundo!" en la consola:

```Gleam
import gleam/io

pub fn main() {
  io.print("¡Hola mundo!")
}
```

Podemos compilar y ejecutar este código utilizando el comando `gleam run` y deberíamos ver el mensaje impreso en la consola.

## Profundizando

Al iniciar un nuevo proyecto en Gleam, también es importante tener en cuenta algunas prácticas recomendadas para mantener nuestro código organizado y escalable. Algunos de estos incluyen:

- Utilizar módulos y namespaces para estructurar nuestro código de manera lógica.
- Escribir pruebas unitarias para asegurarnos de que nuestro código funciona correctamente y poder realizar cambios con confianza.
- Utilizar patrones de concurrencia como actors y procesos para aprovechar al máximo las capacidades de Gleam.
- Familiarizarnos con los diferentes tipos de datos y operadores disponibles en Gleam.

Además, siempre podemos consultar la documentación y unirse a la comunidad de Gleam para obtener más información sobre cómo escribir código eficiente y elegante en este lenguaje.

## Ver también

- Documentación oficial de Gleam: https://gleam.run/
- Comunidad de Gleam en Discord: https://discord.com/invite/gleam-lang
- Ejemplos de código en Gleam: https://github.com/gleam-lang/gleam/tree/master/examples