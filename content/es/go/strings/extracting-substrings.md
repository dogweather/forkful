---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:41.637175-07:00
description: "Extraer subcadenas implica recuperar porciones espec\xEDficas de una\
  \ cadena bas\xE1ndose en sus posiciones. Los programadores realizan frecuentemente\
  \ esta\u2026"
lastmod: '2024-02-25T18:49:55.074024-07:00'
model: gpt-4-0125-preview
summary: "Extraer subcadenas implica recuperar porciones espec\xEDficas de una cadena\
  \ bas\xE1ndose en sus posiciones. Los programadores realizan frecuentemente esta\u2026"
title: Extrayendo subcadenas
---

{{< edit_this_page >}}

## Qué y Por Qué?

Extraer subcadenas implica recuperar porciones específicas de una cadena basándose en sus posiciones. Los programadores realizan frecuentemente esta operación para procesar o manipular datos de texto de manera eficiente, como analizar entradas, validar formatos o preparar salidas.

## Cómo hacerlo:

En Go, el tipo `string` es un segmento de bytes de solo lectura. Para extraer subcadenas, principalmente se utiliza la sintaxis de `slice` (segmento), junto con la función integrada `len()` para la comprobación de longitud y el paquete `strings` para operaciones más complejas. Aquí te mostramos cómo puedes lograrlo:

### Cortes Básicos

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Extrae "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Salida: World
}
```

### Usando el paquete `strings`

Para una extracción de subcadena más avanzada, como extraer cadenas después o antes de una subcadena específica, puedes usar el paquete `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Extrae subcadena después del "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Salida: John Doe
}
```

Es esencial tener en cuenta que las cadenas de Go están codificadas en UTF-8 y un segmento de bytes directo no siempre resultará en cadenas válidas si incluyen caracteres de varios bytes. Para soporte Unicode, considera usar `range` o el paquete `utf8`.

### Manejando Caracteres Unicode

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Encuentra subcadena considerando caracteres Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Salida: 世界
}
```

## Estudio Profundo

Extraer subcadenas en Go es sencillo, gracias a su sintaxis de segmentos y su amplia biblioteca estándar. Históricamente, los lenguajes de programación anteriores proporcionaban funciones o métodos más directos para manejar tal manipulación de texto. Sin embargo, el enfoque de Go enfatiza la seguridad y la eficiencia, particularmente con sus cadenas inmutables y el manejo explícito de caracteres Unicode a través de runas.

Aunque el cortado directo se beneficia de la eficiencia en rendimiento, hereda las complejidades de manejar directamente los caracteres UTF-8. La introducción del tipo `rune` permite que los programas en Go manejen de manera segura el texto Unicode, lo que lo convierte en una alternativa poderosa para aplicaciones internacionales.

Además, los programadores que vienen de otros lenguajes podrían echar de menos funciones integradas de manipulación de cadenas de alto nivel. Sin embargo, los paquetes `strings` y `bytes` en la biblioteca estándar de Go ofrecen un rico conjunto de funciones que, aunque requieren un poco más de estructura, proporcionan opciones poderosas para el procesamiento de cadenas, incluida la extracción de subcadenas.

En esencia, las decisiones de diseño de Go en torno a la manipulación de cadenas reflejan sus objetivos de simplicidad, rendimiento y seguridad al tratar con datos de texto modernos e internacionalizados. Aunque podría requerir un ligero ajuste, Go ofrece herramientas efectivas y eficientes para manejar la extracción de subcadenas y más.
