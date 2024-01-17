---
title:                "Encontrando la longitud de una cadena"
html_title:           "Gleam: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

¡Saludos Geeks de la programación! ¿Están listos para aprender más sobre el fantástico lenguaje Gleam? Hoy vamos a hablar sobre cómo encontrar la longitud de una cadena en Gleam. Así es, ¡sigue leyendo para descubrir todo lo que necesitas saber sobre esta útil tarea en tu código!

## ¿Qué y por qué?
En términos simples, encontrar la longitud de una cadena significa determinar cuántos caracteres hay en esa cadena. Esto es importante para los programadores, porque a menudo necesitamos saber la longitud de una cadena para realizar diferentes operaciones en ella, como dividirla en trozos más pequeños o compararla con otra cadena. En resumen, saber la longitud de una cadena nos permite manipularla y utilizarla de manera más eficiente en nuestro código.

## ¡Vamos a ello!
Ahora que sabemos por qué es importante encontrar la longitud de una cadena, veamos cómo podemos hacerlo en Gleam. Primero, definiremos una cadena y luego aplicaremos la función ```String.length()``` para obtener su longitud. Echa un vistazo al ejemplo de código a continuación:
```
Gleam import gleam/string

let cadena = "¡Hola mundo!"
let longitud = String.length(cadena)

gleam/io.print("La longitud de la cadena es: ", longitud)
```
Después de ejecutar el código, obtendremos la salida: ```La longitud de la cadena es: 12```

## Profundizando
Para aquellos que quieran saber más sobre la longitud de las cadenas en Gleam, aquí hay algunas cosas adicionales que pueden investigar. En primer lugar, esta función no es exclusiva de Gleam, se encuentra en varios lenguajes de programación como Java, Python y C++. Sin embargo, lo que hace que Gleam se destaque es su enfoque en la seguridad del tipo de datos, lo que significa que esta función también maneja caracteres unicode de forma adecuada. También puede explorar otras alternativas para encontrar la longitud de una cadena en Gleam, como el método ```String.code_units()``` que devuelve la cantidad de unidades de código en una cadena.

## ¡Más aprendizaje!
Hay muchas cosas interesantes que puedes aprender sobre Gleam y la programación en general. Aquí hay algunas fuentes adicionales que pueden ser útiles para aquellos que quieran profundizar más en el tema:

- Documentación oficial de Gleam: [https://gleam.run/](https://gleam.run/)
- Tutoriales y ejercicios prácticos: [https://gitlab.com/gleam-lang/gleam-mode](https://gitlab.com/gleam-lang/gleam-mode)
- Comunidad de Gleam en Discord: [https://discord.gg/M64zm58](https://discord.gg/M64zm58)

¡Eso es todo por ahora! Espero que hayas aprendido algo nuevo sobre cómo encontrar la longitud de una cadena en Gleam. ¡Sigue practicando y experimentando para mejorar tus habilidades como desarrollador de Gleam! ¡Hasta pronto! 👋