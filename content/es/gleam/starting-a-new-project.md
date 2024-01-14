---
title:                "Gleam: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador curioso o simplemente quieres probar una nueva tecnología, Gleam es una excelente opción para comenzar un nuevo proyecto. Con su lenguaje funcional elegante y su enfoque en la seguridad del tipo de datos, Gleam te permite escribir código limpio y confiable, ideal para proyectos de cualquier tamaño.

## Cómo hacerlo

Para comenzar a usar Gleam, primero debes instalarlo en tu sistema. Puedes encontrar instrucciones detalladas en la documentación oficial de Gleam. Una vez que esté instalado, puedes crear un nuevo proyecto con el siguiente comando:

```Gleam new my_project```

Esto creará una estructura básica para tu proyecto, incluyendo un archivo de configuración ```gleam.toml```, una carpeta ```src``` para tus archivos de código y una carpeta ```test``` para tus pruebas.

A continuación, comenzaremos creando un módulo simple y una función en nuestro archivo ```src/main.gleam```:

```Gleam
// Importar el módulo "io" para imprimir en consola
import io

// Crear una función que imprima un mensaje de saludo
pub fn saludar(nombre) {
  let mensaje = "¡Hola " ++ nombre ++ "!"
  io.print(mensaje)
}

// Llamar a la función con un nombre específico
saludar("Juan")
```

Al ejecutar nuestro código con el siguiente comando:

```Gleam run src/main.gleam```

Deberíamos ver el mensaje "¡Hola Juan!" impreso en nuestra consola. ¡Felicitaciones, acabas de escribir tu primer programa en Gleam! Ahora puedes explorar más características del lenguaje y continuar construyendo tu proyecto.

## Profundizando

Para obtener más información sobre cómo comenzar un nuevo proyecto en Gleam, te recomendamos revisar la documentación oficial y explorar los diferentes módulos y tipos de datos disponibles. También puedes unirte a la comunidad de Gleam en Discord, donde puedes hacer preguntas y compartir tus proyectos con otros programadores.

## Ver también

- [Documentación oficial de Gleam](https://gleam.run/)
- [Guía de instalación de Gleam](https://gleam.run/getting-started/installation.html)
- [Discord de Gleam](https://discord.gg/pqNV7x2)