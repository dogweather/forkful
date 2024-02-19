---
aliases:
- /es/go/finding-the-length-of-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:40.243373-07:00
description: "Encontrar la longitud de una cadena en Go consiste en determinar el\
  \ n\xFAmero de caracteres que contiene. Los programadores realizan rutinariamente\
  \ esta\u2026"
lastmod: 2024-02-18 23:09:09.444295
model: gpt-4-0125-preview
summary: "Encontrar la longitud de una cadena en Go consiste en determinar el n\xFA\
  mero de caracteres que contiene. Los programadores realizan rutinariamente esta\u2026"
title: Encontrando la longitud de una cadena
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Encontrar la longitud de una cadena en Go consiste en determinar el número de caracteres que contiene. Los programadores realizan rutinariamente esta operación para manipular cadenas de manera efectiva, ya sea para validación, extracción de subcadenas o simplemente para imponer restricciones en las entradas de los usuarios.

## Cómo hacerlo:
En Go, las cadenas se tratan como secuencias de bytes inmutables. Puedes encontrar la longitud de una cadena usando la función incorporada `len()`, que devuelve el número de bytes, no necesariamente el número de caracteres. Así es cómo utilizarla:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Utilizando len() para encontrar la longitud en bytes
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Longitud en Bytes:", byteLength) // Salida: Longitud en Bytes: 13

	// Para obtener con precisión el número de caracteres o runas en una cadena
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Longitud de Runa:", runeLength) // Salida: Longitud de Runa: 9
}
```
El primer método usando `len()` puede que no siempre dé el resultado esperado ya que cuenta bytes. Para cadenas que contienen caracteres no ASCII (como "世界"), se debe utilizar `RuneCountInString` del paquete `unicode/utf8` en su lugar para contar los puntos de código Unicode de manera precisa.

## Análisis profundo
Antes de Go 1, no existía una demarcación estricta para manejar las cadenas como secuencias de bytes versus secuencias de caracteres. Posterior a Go 1, la adopción de UTF-8 como el esquema de codificación estándar para las cadenas necesitó enfoques más claros. La función `len()` funciona perfectamente para cadenas ASCII, donde los caracteres están representados en un solo byte. Sin embargo, a medida que las aplicaciones Go se volvieron más globales y la necesidad de soportar una plétora de idiomas y conjuntos de caracteres creció, el enfoque simplista de `len()` mostró limitaciones.

La introducción y uso de `utf8.RuneCountInString()` responden a estas limitaciones al proporcionar una manera de contar caracteres Unicode reales (runas en la terminología de Go). Este método asegura que el cálculo de la longitud sea independiente de los detalles de codificación específicos de UTF-8, donde los caracteres podrían abarcar múltiples bytes.

Un enfoque alternativo para atravesar y manipular cadenas, más en línea con el espíritu de concurrencia y eficiencia de Go, podría implicar tratar las cadenas como rebanadas de runas. Sin embargo, este método requiere un paso de conversión y no resuelve instantáneamente todas las complejidades de Unicode (por ejemplo, caracteres combinados).

En resumen, mientras que `len()` es adecuado para la longitud en bytes y es eficiente para texto ASCII, `utf8.RuneCountInString()` es una opción más confiable para una aplicación compatible globalmente. Sin embargo, se alienta a los desarrolladores a entender los compromisos en rendimiento y uso de memoria que estas opciones conllevan.
