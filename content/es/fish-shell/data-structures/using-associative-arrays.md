---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:53.655418-07:00
description: "Los arreglos asociativos, o mapas hash, te permiten almacenar datos\
  \ como pares clave-valor, facilitando la organizaci\xF3n y recuperaci\xF3n de informaci\xF3\
  n por\u2026"
lastmod: '2024-03-11T00:14:33.327867-06:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, o mapas hash, te permiten almacenar datos como\
  \ pares clave-valor, facilitando la organizaci\xF3n y recuperaci\xF3n de informaci\xF3\
  n por\u2026"
title: Uso de matrices asociativas
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Los arreglos asociativos, o mapas hash, te permiten almacenar datos como pares clave-valor, facilitando la organización y recuperación de información por clave. Son útiles cuando necesitas una forma más estructurada de manejar datos que solo listas, especialmente en configuraciones y al tratar con un rango de atributos.

## Cómo hacerlo:

Fish no soporta nativamente arreglos asociativos como Bash 4+, pero puedes lograr una funcionalidad similar usando una combinación de listas y manipulación de cadenas. Así es cómo imitarlos:

Primero, configurando elementos de "arreglo asociativo" por separado:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Para acceder a un elemento, solo refiérelo directamente:

```Fish Shell
echo $food_color_apple
# Salida: red
```

Si necesitas iterar sobre ellos, usa un bucle for considerando una convención de nombres:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Salida:
# red
# yellow
```

Para aquellos que extrañan el `${!array[@]}` de Bash para obtener todas las claves, puedes almacenar las claves en una lista separada:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'es' $food_color_$key
end
# Salida:
# apple es red
# banana es yellow
```

## Estudio Profundo

Los verdaderos arreglos asociativos como en otros lenguajes de scripting aún no son parte del enfoque de Fish. El truco mostrado aprovecha la manipulación de cadenas y las capacidades de lista de Fish para crear una estructura pseudo-asociativa. Aunque funciona, no es tan limpio ni a prueba de errores como lo sería el soporte integrado de arreglos asociativos. Otros shells como Bash y Zsh ofrecen funcionalidad de arreglos asociativos integrada, lo que resulta en un código más directo y legible. Sin embargo, la filosofía de diseño de Fish apunta a la simplicidad y amigabilidad con el usuario, posiblemente a expensas de tales características. El truco satisface la mayoría de las necesidades pero mantén un ojo en la evolución de Fish Shell—sus desarrolladores mejoran activamente y añaden características basadas en los comentarios de la comunidad.
