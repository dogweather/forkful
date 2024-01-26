---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:55:59.415630-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Leer argumentos de la línea de comandos permite a tu programa recibir datos o instrucciones al ejecutarse. Los programadores lo usan para hacer que los programas sean flexibles y adaptables a diferentes situaciones y necesidades del usuario.

## Cómo:
```Gleam
import gleam/io
import gleam/list.{map, Intersperse}

fn main(args: List(String)) {
  let args_with_indexes = list.map(args, fn(arg) { 
    arg 
  })
  .intersperse(" ")
  .to_string()
  
  io.println("Argumentos recibidos: " ++ args_with_indexes)
}

// Supongamos que tu programa se llama 'mi_app'.
// Ejecuta en la terminal: ./mi_app estos son los argumentos
// Salida esperada: Argumentos recibidos: estos son los argumentos
```

## Profundización
Históricamente, leer argumentos de la línea de comandos es una práctica que proviene de los primeros días de UNIX donde la interacción con la terminal era esencial. Alternativamente, puedes usar archivos de configuración o variables de entorno, pero para acciones directas y rápidas, los argumentos son insuperables. En Gleam, utilizamos las funciones del módulo `gleam/io` para interactuar con la entrada/salida y el manejo de listas para procesar los argumentos recibidos.
