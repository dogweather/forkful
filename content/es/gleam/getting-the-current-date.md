---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:14:22.576829-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en programación significa capturar el momento presente del sistema. Es útil para registros, marcas de tiempo y funcionalidades dependientes del tiempo.

## Cómo hacerlo:
Para obtener la fecha actual en Gleam, primero asegúrate de tener las dependencias necesarias. Aquí te muestro cómo:

```gleam
import gleam/io
import gleam/erlang/time

pub fn main() {
  let now = time.now()
  io.println(now)
}
```

Ejecuta tu código y obtendrás algo parecido a esto:

```
{{2023, 4, 14}, {12, 34, 56}}
```

## Profundizando
Históricamente, la obtención de fechas en lenguajes de programación ha evolucionado para manejar complejidades temporales (zonas horarias, calendarios, etc.). En Gleam, un lenguaje funcional que compila a Erlang, aprovechamos la robustez que proporciona la Beam VM, la máquina virtual en la que también corren Erlang y Elixir.

Existen alternativas a la función `time.now()`, como el uso de bibliotecas externas o funciones orientadas a formatos más específicos o cálculos de tiempo avanzados. Sin embargo, para la mayoría de usos, `time.now()` ofrecerá precisión y eficiencia.

Detalles de la implementación incluyen el manejo de la precisión de tiempo (normalmente en la escala de milisegundos), gestión de zonas horarias, y la posibilidad de usar timestamps UNIX para una representación más universal.

## Ver También
- [Gleam Language Documentation](https://gleam.run/)
- [Erlang Time Module](http://erlang.org/doc/man/erlang.html#date-0)