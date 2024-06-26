---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:42.941084-07:00
description: "C\xF3mo hacerlo: En Rust, el tipo `HashMap` del m\xF3dulo `std::collections`\
  \ proporciona la funcionalidad de los arreglos asociativos. As\xED es c\xF3mo puedes\u2026"
lastmod: '2024-03-13T22:44:58.839491-06:00'
model: gpt-4-0125-preview
summary: "En Rust, el tipo `HashMap` del m\xF3dulo `std::collections` proporciona\
  \ la funcionalidad de los arreglos asociativos."
title: Uso de matrices asociativas
weight: 15
---

## Cómo hacerlo:
En Rust, el tipo `HashMap` del módulo `std::collections` proporciona la funcionalidad de los arreglos asociativos. Así es cómo puedes trabajar con ellos:

```Rust
use std::collections::HashMap;

fn main() {
    // Creando un nuevo HashMap
    let mut puntuaciones = HashMap::new();

    // Insertando valores
    puntuaciones.insert(String::from("Azul"), 10);
    puntuaciones.insert(String::from("Amarillo"), 50);

    // Accediendo a los valores
    let nombre_del_equipo = String::from("Azul");
    if let Some(puntuación) = puntuaciones.get(&nombre_del_equipo) {
        println!("Puntuación para el equipo Azul: {}", puntuación); // Salida: Puntuación para el equipo Azul: 10
    }

    // Actualizando un valor
    puntuaciones.entry(String::from("Azul")).and_modify(|e| *e += 5);

    // Iterando sobre pares clave-valor
    for (clave, valor) in &puntuaciones {
        println!("{}: {}", clave, valor); // Salida: Azul: 15, Amarillo: 50
    }
}
```

## Estudio Profundo
El `HashMap` en Rust utiliza una función hash para mapear claves a valores, lo que permite una rápida recuperación de datos. Sin embargo, esta eficiencia tiene un costo: los mapas hash no mantienen el orden de sus elementos. Esto contrasta con otras implementaciones de arreglos asociativos, como los de Python (`dict`) o Ruby, que en versiones recientes mantienen el orden de inserción como una característica. Para casos de uso donde el orden de los pares clave-valor es significativo, los desarrolladores de Rust podrían considerar usar el `BTreeMap` del módulo `std::collections`, el cual mantiene el orden pero podría ofrecer una inserción y recuperación más lentas en comparación con `HashMap`. En última instancia, la elección entre `HashMap` y `BTreeMap` depende de requisitos específicos en torno al ordenamiento y rendimiento.
