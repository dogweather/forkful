---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Búsqueda y reemplazo de texto con Bash

## ¿Qué & Por qué?

La búsqueda y el reemplazo de texto implican encontrar cadenas de caracteres específicas en un texto y cambiarlas por otras. Los programadores lo hacen para modificar código rápidamente, corregir errores y adaptar scripts a diferentes contextos.

## ¿Cómo hacerlo?

Aquí te muestro cómo buscar y reemplazar texto en Bash. 

Reemplazaremos la palabra "hola" por "adiós" en nuestro archivo de texto.

```Bash
sed 's/hola/adiós/g' tu_archivo.txt
```

Aquí, `s` significa 'substituir'. `g` al final indica que queremos reemplazar todas las ocurrencias, no solo la primera. 

Este es solo un ejemplo básico. Bash ofrece muchas formas de afinar tus búsquedas y reemplazos para adaptarse a las necesidades del proyecto.

## Más a fondo

El comando `sed` viene del editor de flujos (stream editor) que es una herramienta estandarizada en Unix desde los años 70. 

Existen alternativas a `sed`, como `awk` o `perl`, que a veces pueden ser más aptas o cómodas según el caso.

 Los detalles de implementación de `sed` son profundos, pero aquí destacamos que `sed` no modifica el archivo original, hace la operación en su copia cargada en memoria. Si quieres que los cambios se guarden en el archivo original, utiliza el flag `-i`:

```Bash
sed -i 's/hola/adiós/g' tu_archivo.txt
```

## Ver también: 

Para profundizar en `sed` y sus alternativas:
- `sed` - Tutorial y referencia completa: https://www.grymoire.com/Unix/Sed.html
- `awk` - Tutorial y referencia completa: https://www.grymoire.com/Unix/Awk.html
- `perl` – Tutorial y referencia completa: https://perldoc.perl.org/ 
- Bash Scripting Guide: https://tldp.org/LDP/abs/html/textmanipulation.html 

Recuerda, la práctica hace al maestro. ¡Feliz programación!