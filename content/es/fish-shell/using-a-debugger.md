---
title:                "Usando un depurador"
date:                  2024-01-26T03:48:34.192288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Usar un depurador se trata de aplastar bugs, esos desagradables y consumidores de tiempo errores en tu código. Los programadores depuran porque quieren encontrar y arreglar problemas de manera eficiente, entender el flujo del código y obtener una imagen más clara de lo que su código realmente está haciendo.

## Cómo hacerlo:
Fish no tiene un depurador integrado como otras shells, pero puedes usar herramientas externas como `gdb` para depurar programas compilados o `fish -d` para ejecutar fish con salida de depuración en diferentes niveles. Vamos a proceder con `fish -d`:

```fish
# Ejecuta fish shell con nivel de depuración 2
fish -d2

# En la shell fish, vamos a probar una simple función con un posible bug
function test_func
    set val 42
    echo "El valor es $val"
    if test $val -eq 42
        echo "Todo está bien."
    else
        echo "Algo huele a pescado."
    end
end

# Llama a la función y observa la salida de depuración
test_func
```

Verías una salida de depuración extra antes y después de la ejecución de la función, ayudándote a identificar problemas.

## Inmersión Profunda
Históricamente, la depuración en entornos similares a Unix ha sido una provincia de herramientas especializadas como `gdb` para C/C++ o `pdb` para Python. En Fish, usualmente dependes de utilidades externas o características integradas como `functions -v` para una salida detallada de funciones y `set -x` para rastrear cambios en las variables.

Algunas personas eligen shells alternativas como Bash debido a características como `set -x` para depurar guiones. Sin embargo, Fish tiene su encanto con un enfoque en la facilidad de uso e interactividad, lo cual puede reducir la necesidad de depuración intensiva en muchos casos.

Cuando se trata de implementación, depurar un guion a menudo implica ejecutarlo con salida detallada y rastrear dónde las variables se establecen, desestablecen o mutan de maneras inesperadas. Con la salida codificada por colores de Fish y un enfoque amigable para el usuario, a menudo puedes evitar lo tedioso de la depuración - pero cuando estás atascado, recuerda que la verbosidad y claridad son tus mejores herramientas.

## Ver También
Aquí tienes algunas líneas de vida confiables cuando estás hasta las aletas en código:

- Documentación de Fish sobre depuración: https://fishshell.com/docs/current/index.html#debugging
- Guía oficial del GDB (GNU Debugger): https://www.gnu.org/software/gdb/documentation/
- Etiqueta Fish en Stack Overflow - casos de depuración en el mundo real: https://stackoverflow.com/questions/tagged/fish
- Guía Avanzada de Scripting en Bash - para comparar enfoques de depuración: https://tldp.org/LDP/abs/html/debugging.html
