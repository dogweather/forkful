---
title:    "Swift: Capitalizando una cadena"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Swift?

Capitalizar una cadena en Swift puede ser útil en varias situaciones, como en la creación de titulares o títulos para presentaciones, formularios o contenido de una aplicación. También puede ser necesario para seguir ciertos estándares de escritura o simplemente para mejorar la presentación visual de una cadena. En este post, aprenderás cómo capitalizar una cadena en Swift y profundizarás en el proceso detrás de esta tarea.

## Cómo hacerlo

Para capitalizar una cadena en Swift, se puede utilizar el método `capitalized`, el cual devuelve una nueva cadena con la primera letra de cada palabra en mayúscula. Además, se pueden utilizar otros métodos como `uppercased` y `lowercased` para convertir toda la cadena en mayúsculas o minúsculas, respectivamente. A continuación, se muestra un ejemplo de código usando estos métodos:

```Swift
let name = "juan perez"

let capitalized = name.capitalized
print(capitalized) // Salida: Juan Perez

let uppercase = name.uppercased()
print(uppercase) // Salida: JUAN PEREZ

let lowercase = name.lowercased()
print(lowercase) // Salida: juan perez
```

También se pueden combinar estos métodos para lograr un formato específico en la cadena. Por ejemplo, si se quiere que solo la primera letra sea mayúscula y el resto en minúscula, se puede utilizar el siguiente código:

```Swift
let name = "juan perez"

let formattedName = name.prefix(1).uppercased() + name.dropFirst().lowercased()
print(formattedName) // Salida: Juan perez
```

En este caso, se utiliza el método `prefix` para obtener la primera letra de la cadena y luego se encadena con el método `lowercased` para el resto de la cadena. Esto es solo un ejemplo, se pueden utilizar diferentes combinaciones de métodos según se requiera.

## Profundizando en la capitalización de cadenas

Cuando se utiliza el método `capitalized`, se asume que las palabras están separadas por un espacio. Sin embargo, ¿qué pasa si hay palabras que no están separadas o si se quiere que ciertas palabras no sean capitalizadas? En estos casos, se pueden utilizar diferentes enfoques para lograr el resultado deseado.

Una opción es utilizar un diccionario de palabras que no deben ser capitalizadas y agregar lógica al código para manejar estas excepciones. Otra opción es utilizar expresiones regulares para detectar y reemplazar ciertos patrones en la cadena antes de capitalizarla.

También es importante tener en cuenta las diferentes reglas de capitalización en diferentes idiomas y tener en cuenta la codificación de caracteres para manejar adecuadamente los acentos y símbolos.

## Ver también

- [Documentación de Apple sobre el método `capitalized`](https://developer.apple.com/documentation/swift/string/2427850-capitalized)
- [Guía para trabajar con cadenas en Swift](https://plataforma.munigratuito.pe/es/blog/articulo/swift-guia-para-trabajar-con-cadenas/)
- [Expresiones regulares en Swift](https://www.swift.com.co/why-swift/swift-articles/expresiones-regulares-en-swift)

Espero que este artículo te haya ayudado a comprender cómo capitalizar una cadena en Swift y te sea útil en tus proyectos. ¡Happy coding! 🚀