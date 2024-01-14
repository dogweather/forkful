---
title:                "Swift: Analizar HTML"
simple_title:         "Analizar HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Hoy en día, la información se encuentra en todas partes en la web, pero a veces no es tan fácil de leer o utilizar como nos gustaría. Ahí es donde entra la habilidad de parsear HTML. Al aprender esta técnica, podrás extraer datos de sitios web y utilizarlos en tus propias aplicaciones sin tener que copiar y pegar manualmente. Además, conocer cómo funciona el HTML subyacente puede ayudarte a mejorar tus habilidades de desarrollo web.

## Cómo hacerlo

A continuación, compartiremos un ejemplo sencillo de cómo se puede hacer un parser de HTML en Swift utilizando la biblioteca HTMLKit. En primer lugar, debes importar la biblioteca en tu proyecto:

```Swift
import HTMLKit
```

Luego, necesitamos acceder al sitio web de donde queremos extraer datos y obtener el contenido HTML:

```Swift
let url = "https://ejemplo.com"
guard let htmlData = try? Data(contentsOf: URL(string: url)!) else { return }
```

Con esto, tenemos el contenido HTML almacenado en la variable "htmlData". Ahora, utilizaremos la función de parse de HTMLKit para convertir el contenido en formato HTML a un objeto HTMLNode que podamos tratar y manipular:

```Swift
let parser = HTMLParser(data: htmlData)
let document = parser.parse()
```

Una vez que tenemos el objeto HTMLNode, podemos utilizar métodos como "querySelectorAll" o "querySelector" para buscar elementos específicos en el HTML utilizando selectores CSS. Por ejemplo, si queremos obtener el título de la página, podemos utilizar el selector "head > title" de la siguiente manera:

```Swift
guard let title = document?.querySelector("head > title")?.text else { return }
print(title) // Imprime: "Ejemplo - Página principal"
```

## Profundizando más

Parsificar HTML puede ser un tema complejo y existen muchas maneras de hacerlo en Swift. Si quieres profundizar más en este tema, te recomendamos revisar la documentación de HTMLKit y otras bibliotecas similares como SwiftSoup.

Otra opción es aprender a hacer un parser personalizado utilizando expresiones regulares. Aunque puede ser más complejo, te permite tener un mayor control sobre cómo se obtienen y procesan los datos.

Recuerda también que es importante tener en cuenta la estructura del HTML y utilizar selectores adecuados para obtener los datos deseados.

## Ver también
- [Documentación de HTMLKit](https://htmlkit.com/docs/)
- [Documentación de SwiftSoup](https://github.com/scinfu/SwiftSoup)
- [Tutorial de parsing de HTML con expresiones regulares en Swift](https://www.raywenderlich.com/3494-nsregularexpression-tutorial-and-cheat-sheet#toc-anchor-017)