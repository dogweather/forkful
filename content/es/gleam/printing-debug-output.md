---
title:                "Imprimiendo salida de depuración"
date:                  2024-01-20T17:52:55.657567-07:00
model:                 gpt-4-1106-preview
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Imprimir mensajes para depuración es mostrar datos en la consola para entender qué está pasando en tu código. Los programadores lo hacen para rastrear errores o comportamientos inesperados de forma rápida y sencilla.

## Cómo hacerlo:
Gleam usa la función `println` para imprimir en la consola. Aquí tienes un ejemplo sencillo:

```gleam
pub fn main() {
  let my_debug_data = "¡Hola depuración!";
  println(my_debug_data);
}
```

Salida esperada:

```
¡Hola depuración!
```

Puedes imprimir cualquier cosa que derive de `Debug`:

```gleam
pub fn main() {
  let numbers = [1, 2, 3];
  println(numbers);
}
```

Salida esperada:

```
[1, 2, 3]
```

## Inmersión Profunda:
Imprimir mensajes de depuración es una práctica tan antigua como la programación misma. Antes de las interfaces gráficas y herramientas avanzadas, era la principal manera de entender qué estaba fallando.

Otras maneras de depurar incluyen el uso de depuradores paso a paso o escribir pruebas automatizadas. La ventaja de imprimir es que es rápido y directo. En Gleam, como en muchos lenguajes funcionales, la depuración a través de impresiones es crucial debido a la inmutabilidad de los datos, lo que a veces puede complicar el seguimiento del flujo del programa.

La función `println` envía salida al `stdout` del sistema, lo cual es estándar entre lenguajes. Sin embargo, abusa de este método y tu consola será un caos, así que úsalo con moderación y recuerda limpiar esos mensajes una vez resuelto el problema.

## Véase También:
- Artículo sobre "Debugging in Functional Languages": [Debugging in Functional Languages](https://wiki.haskell.org/Debugging)
- Un tutorial sobre depuración en Erlang (cercanamente relacionado con Gleam): [Erlang Debugging](http://erlang.org/doc/apps/debugger/debugger_chapter.html)
