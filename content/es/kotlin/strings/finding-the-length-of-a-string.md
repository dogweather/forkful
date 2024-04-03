---
date: 2024-01-20 17:47:52.531793-07:00
description: "C\xF3mo se hace: Kotlin lo hace simple. Usa `.length`. As\xED."
lastmod: '2024-03-13T22:44:59.026841-06:00'
model: gpt-4-1106-preview
summary: Kotlin lo hace simple.
title: Calculando la longitud de una cadena
weight: 7
---

## Cómo se hace:
Kotlin lo hace simple. Usa `.length`. Así:

```Kotlin
fun main() {
    val texto = "Hola Mundo"
    println("La longitud de la cadena es: ${texto.length}")
}
```

Salida:
```
La longitud de la cadena es: 10
```

## Inmersión Profunda
Históricamente, la longitud de una cadena siempre ha sido un dato crítico en la programación. En Kotlin, `.length` está disponible en todas las clases que representan secuencias de caracteres. Alternativamente, podrías iterar sobre la cadena y contar los caracteres uno por uno, pero ¿por qué reinventar la rueda? `.length` es una propiedad de la clase String que hace justo eso, de manera eficiente.

Detrás de escena, `.length` devuelve un entero que representa el número de caracteres en la cadena. Pero cuidado con los caracteres Unicode que pueden ser representados por más de un `Char`: Kotlin cuenta estos como dos caracteres diferentes por cómo están codificados en UTF-16.

## Ver También
- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)y la explicación de las cadenas de texto.
- [Unicode y UTF-16](https://unicode.org/faq/utf_bom.html): Entender problemas de codificación al trabajar con strings.
