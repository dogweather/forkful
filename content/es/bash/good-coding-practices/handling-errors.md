---
title:                "Manejo de errores"
aliases: - /es/bash/handling-errors.md
date:                  2024-01-26T00:37:18.314422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Manejar errores en scripts de Bash consiste en anticipar dónde pueden surgir problemas y abordarlos de manera elegante. ¿Por qué? Bueno, mantiene tu script robusto y ahorra a los usuarios el tener que romperse la cabeza cuando las cosas no funcionan como se esperaba.

## Cómo hacerlo:

```Bash
#!/bin/bash

# Redirigiendo stderr a un archivo
grep "algo" archivo.txt 2> errores.log

# Manejo de errores con estados de salida
if ! grep "algo" archivo.txt; then
    echo "Uy, algo salió mal buscando 'algo'."
    exit 1
fi

# Usando un trap para limpiar antes de salir por un error
limpiar() {
  echo "Limpiando archivos temporales..."
  rm temp_*
}

trap limpiar ERR

# error intencionado: el archivo no existe
cat temp_archivo.txt
```

Salida de muestra cuando ocurre un error:

```
Limpiando archivos temporales...
cat: temp_archivo.txt: No existe el archivo o el directorio
```

## Análisis Profundo

El manejo de errores en scripts de Bash se remonta a los orígenes de la shell de Unix, donde los scripts robustos y fiables eran (y son) vitales para la administración del sistema y la automatización. Tradicionalmente, los errores en Bash se manejan verificando el estado de salida de un comando, que por convención devuelve 0 para éxito y un valor distinto de cero para fallo.

Bash introdujo el comando `trap` como una función incorporada, permitiendo a los usuarios especificar comandos que se ejecutarían con diversas señales o salidas del script. Esto es útil para tareas de limpieza o como un mecanismo de manejo de errores de último recurso.

También está el comando `set`, que puede cambiar el comportamiento de Bash ante errores. Por ejemplo, `set -e` hará que un script salga inmediatamente si cualquier comando sale con un estado distinto de cero, una manera de fallar rápidamente y evitar errores en cascada.

Las alternativas al manejo de errores integrado en Bash incluyen la comprobación explícita de la existencia de archivos, el uso de sustitución de comandos o incluso escribir tus propias funciones para manejar errores de manera más granular.

Aunque el manejo riguroso de errores a veces puede parecer excesivo para scripts pequeños, es una práctica que puede ahorrar mucho tiempo de depuración y prevenir comportamientos inesperados tanto para ti como para los usuarios.

## Ver También

- Manual de Bash sobre Parámetros de Shell: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Sección de la Guía de Scripting-Avanzado en Bash sobre Manejo de Errores: https://www.tldp.org/LDP/abs/html/exit-status.html
- Una guía detallada sobre `trap`: https://mywiki.wooledge.org/SignalTrap

Recuerda, escribir scripts es una forma de arte, y cómo manejas los resbalones y tropezones puede hacer que tu obra maestra sea más resistente. ¡Feliz scripteo!
