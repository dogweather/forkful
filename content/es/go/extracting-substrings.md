---
title:    "Go: Extrayendo subcadenas"
keywords: ["Go"]
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Go?

Extraer subcadenas es una tarea común en la programación, especialmente en Go. Puede ser útil cuando se trabaja con cadenas de texto largas y solo se necesita una parte específica de ella, como un número de teléfono o una dirección de correo electrónico. En esta publicación, aprenderemos cómo extraer subcadenas en Go y por qué es una habilidad importante para cualquier programador.

## Cómo extraer subcadenas en Go

Extraer subcadenas en Go es muy sencillo gracias al paquete incorporado "strings". Primero, importamos el paquete en nuestro código:

```Go
import "strings"
```

Luego, podemos usar la función "Substring" para extraer una parte de una cadena. Por ejemplo, si queremos obtener solo los últimos 5 dígitos de un número de teléfono, podemos hacer lo siguiente:

```Go
phone := "555-123-4567"
lastDigits := string.Substr(phone, 8, 5)
fmt.Println(lastDigits)
```

El resultado sería "4567", ya que comenzamos a contar desde cero y seleccionamos los últimos 5 caracteres de la cadena original.

Otra forma de extraer una subcadena es utilizando la función "Index" del paquete "strings". Esta función devuelve la posición de un determinado carácter en una cadena. Podemos combinarla con la función "Substring" para obtener una subcadena basada en esa posición. Por ejemplo, si queremos extraer la primera palabra de una oración, podemos hacer lo siguiente:

```Go
sentence := "Esta es una oración de ejemplo."
firstWordEnd := strings.Index(sentence, " ") // obtiene la posición del primer espacio
firstWord := sentence.Substring(0, firstWordEnd)
fmt.Println(firstWord)
```

El resultado sería "Esta".

## Profundizando en la extracción de subcadenas

Además de las funciones mencionadas anteriormente, el paquete "strings" también ofrece otras opciones para trabajar con subcadenas. Podemos encontrar funciones como "Split" para dividir una cadena en varias subcadenas basadas en un separador, "Replace" para reemplazar una subcadena con otra y "Count" para contar cuántas veces aparece una determinada subcadena en una cadena.

También podemos usar "Regular Expressions" para una extracción más precisa y compleja de subcadenas. Go tiene un paquete incorporado "regexp" para trabajar con expresiones regulares.

## Ver también

- [Documentación oficial de Go sobre el paquete "strings"](https://golang.org/pkg/strings/)
- [Ejemplos de código para extraer subcadenas en Go](https://github.com/golang/example/tree/master/strings)
- [Tutorial sobre expresiones regulares en Go](https://regex-golang.appspot.com/)

¡Con estas herramientas y conocimientos, estarás listo para extraer subcadenas en Go como un profesional!