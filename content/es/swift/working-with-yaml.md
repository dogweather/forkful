---
title:                "Swift: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

# Por qué trabajar con YAML en Swift

Si eres un desarrollador de Swift, es posible que hayas escuchado sobre el formato YAML y te preguntes por qué deberías usarlo en tus proyectos. Bueno, ¡estás en el lugar correcto! En esta publicación de blog, vamos a explorar por qué YAML puede ser una herramienta útil para los programadores de Swift.

## Cómo utilizar YAML en Swift

Primero, ¿qué es YAML? YAML (siglas de YAML Ain't Markup Language) es un formato de serialización de datos legibles por humanos. En otras palabras, es una forma de almacenar datos en un formato que es fácilmente comprensible para nosotros los humanos. Pero, ¿cómo podemos usarlo en Swift?

¡Muy sencillo! Swift cuenta con una librería llamada `Yams` que nos permite trabajar con YAML en nuestro código. Echemos un vistazo a un ejemplo de cómo se vería un archivo YAML y cómo podemos manejarlo en Swift:

```Swift
let yamlString = """
person:
  name: María
  age: 28
  profession: Desarrolladora
"""

do {
  let person = try YAMLDecoder().decode(Person.self, from: yamlString)
  print(person) // Person(name: "María", age: 28, profession: "Desarrolladora")
} catch {
  print(error)
}
```

En este ejemplo, hemos creado una estructura `Person` con las propiedades `name`, `age` y `profession`. Luego, hemos utilizado `YAMLDecoder` para decodificar el archivo YAML en un objeto `Person` utilizando el método `decode` y el string YAML como parámetro. Finalmente, imprimimos el objeto `person` y obtenemos el resultado esperado.

## Profundizando en YAML

Hasta ahora, hemos visto cómo podemos trabajar con YAML en Swift, pero ¿por qué es una herramienta útil en la programación? Un beneficio importante es que YAML es legible por humanos, lo que lo hace más fácil de entender y modificar en comparación con otros formatos de datos como JSON o XML. Además, YAML permite comentarios en el archivo, lo que puede ser útil para explicar el propósito de ciertos datos o proporcionar documentación.

Otra ventaja es la capacidad de anidar datos en YAML. Como se vio en nuestro ejemplo anterior, podemos tener propiedades y valores dentro de un objeto `person`, y a su vez, tener ese objeto dentro de otro objeto como `person` dentro de `team`. Esto hace que YAML sea muy útil para estructurar y organizar datos complejos.

## Ver también

Si estás interesado en aprender más sobre YAML en Swift, aquí hay algunos recursos útiles:

- [Documentación de Swift sobre YAML](https://developer.apple.com/documentation/swift/codable/using_your_own_types_for_custom_encodings#3339445)
- [Página de GitHub de la librería Yams](https://github.com/jpsim/Yams)
- [Tutorial de raywenderlich sobre YAML en Swift](https://www.raywenderlich.com/7747260-yaml-tutorial-for-swift-getting-started)

Ahora que tienes una idea de por qué y cómo trabajar con YAML en Swift, ¡puedes comenzar a utilizarlo en tus proyectos y aprovechar sus beneficios! ¡Feliz codificación!