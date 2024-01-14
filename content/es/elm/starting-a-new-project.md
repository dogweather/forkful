---
title:    "Elm: Comenzando un nuevo proyecto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Por qué empezar un nuevo proyecto en Elm?

Empezar un proyecto en Elm puede ser una gran decisión para desarrolladores interesados en herramientas modernas para el desarrollo web. Con su robusto sistema de tipos y sus funciones puras, Elm ofrece una forma elegante y segura de construir aplicaciones.

## Cómo hacerlo

Para empezar un nuevo proyecto en Elm, primero debes asegurarte de tener instalado Elm en tu computadora. Puedes descargarlo desde su página oficial o utilizar un gestor de paquetes como npm. Una vez instalado, puedes crear un nuevo proyecto con el comando ```elm init```. Esto creará una estructura de directorios básica para tu proyecto.

A continuación, necesitas decidir qué tipo de aplicación quieres construir. Para aplicaciones web, puedes utilizar el framework web de Elm llamado "elm-live" o simplemente utilizar las funciones proporcionadas por Elm para interactuar con el DOM. Para aplicaciones móviles, puedes utilizar "elm-native", que te permite escribir código en Elm y compilarlo a aplicaciones nativas para iOS y Android.

Una vez seleccionado el tipo de aplicación, puedes empezar a codificar. Veamos un ejemplo simple de una aplicación que imprime "¡Hola mundo!" en la consola.

```
import Console

main =
  Console.log "¡Hola mundo!"
```

Al compilar y ejecutar este código, verás el mensaje "¡Hola mundo!" en la consola del navegador. Puedes continuar añadiendo más funcionalidades a tu aplicación utilizando los módulos y funciones proporcionados por Elm.

## Profundizando

Si quieres profundizar en la creación de un proyecto en Elm, hay algunas cosas que debes tener en cuenta. En primer lugar, debes familiarizarte con la arquitectura de modelo-vista-actualización de Elm. Entender cómo funciona esta arquitectura te ayudará a escribir código de manera más efectiva.

También es importante recordar que Elm es un lenguaje funcional puro, lo que significa que toda función debe ser pura y no tener efectos secundarios. Esto asegura que tu código sea más predecible y fácil de debuggear.

Por último, asegúrate de utilizar la documentación oficial de Elm y la comunidad en línea para ayudarte en tu proyecto. Puedes encontrar ejemplos de código, tutoriales y consejos en línea para seguir aprendiendo y mejorando tus habilidades en Elm.

## Ver también

- [Página oficial de Elm](https://elm-lang.org/es/)
- [Documentación de Elm](https://elm-lang.org/docs)
- [Comunidad de Elm en Reddit](https://www.reddit.com/r/elm/)
- [Elm en acción: Creando una aplicación web de listas de tareas](https://fireship.io/lessons/elm-in-action-create-a-ticking-digital-clock/) (en inglés)