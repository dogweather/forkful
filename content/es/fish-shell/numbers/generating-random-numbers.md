---
date: 2024-01-27 20:33:29.831021-07:00
description: "C\xF3mo: Generar un n\xFAmero aleatorio en Fish puede ser sencillo,\
  \ utilizando la combinaci\xF3n de utilidades del sistema y capacidades del shell.\
  \ A continuaci\xF3n\u2026"
lastmod: '2024-03-13T22:44:59.495596-06:00'
model: gpt-4-0125-preview
summary: "Generar un n\xFAmero aleatorio en Fish puede ser sencillo, utilizando la\
  \ combinaci\xF3n de utilidades del sistema y capacidades del shell."
title: "Generaci\xF3n de n\xFAmeros aleatorios"
weight: 12
---

## Cómo:
Generar un número aleatorio en Fish puede ser sencillo, utilizando la combinación de utilidades del sistema y capacidades del shell. A continuación se presentan algunos ejemplos que demuestran cómo generar números aleatorios dentro de rangos especificados.

**Generar un número aleatorio entre 0 y 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Salida de muestra:**
```fish
42
```

**Generar un número aleatorio entre dos números cualesquiera, digamos 50 y 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Salida de muestra:**
```fish
103
```

**Usando random para barajar una lista:**

También puede querer barajar aleatoriamente elementos en una lista. Así es cómo puede hacerlo:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Salida de muestra:**
```fish
C
A
E
D
B
```

Por favor, tenga en cuenta que la salida variará cada vez que ejecute estos comandos debido a la naturaleza de la aleatoriedad.

## Profundización
La función `random` de Fish Shell proporciona una interfaz fácil de usar para generar números pseudo-aleatorios. Internamente, se encarga de utilizar herramientas de generación de números aleatorios a nivel de sistema, ofreciendo una forma portátil de introducir aleatoriedad en sus scripts. Sin embargo, es esencial recordar que la aleatoriedad proporcionada por `random` es suficiente para la mayoría de las tareas de scripting pero puede no cumplir con los requisitos de seguridad criptográfica para aplicaciones que necesitan un grado más alto de imprevisibilidad.

Para contextos de seguridad de alto riesgo, considere usar herramientas dedicadas o bibliotecas de programación diseñadas para fines criptográficos, que proporcionan garantías de aleatoriedad más fuertes. Sin embargo, para scripts generales y aplicaciones donde los estándares de seguridad más altos para la aleatoriedad no son un requisito, la función `random` de Fish Shell ofrece una solución conveniente y efectiva.
