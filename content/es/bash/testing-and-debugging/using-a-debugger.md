---
title:                "Usando un depurador"
aliases:
- /es/bash/using-a-debugger/
date:                  2024-01-26T03:47:23.601844-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Usar un depurador en Bash significa aprovechar herramientas para probar y encontrar problemas en tus scripts, como atrapar errores que hacen que tu código se bloquee o se comporte de manera incorrecta de forma sigilosa. Los programadores lo hacen porque es mucho más inteligente capturar errores antes de que causen estragos en un entorno en vivo.

## Cómo hacerlo:
Bash no viene con un depurador incorporado como algunos otros lenguajes, pero puedes usar comandos integrados como `set -x` para rastrear lo que está sucediendo. O, para una mejora, está `bashdb`, un depurador apropiado para avanzar paso a paso por tu código. Aquí hay un vistazo:

```Bash
# Usando set -x para depurar
set -x
echo "Iniciando depuración"
my_var="¡Hola, mundo de la depuración!"
echo $my_var
set +x

# Usando bashdb
# Instala bashdb con tu gestor de paquetes, por ejemplo, apt, yum, brew.
# Depura un script llamado my_script.sh:
bashdb my_script.sh
```

Salida al ejecutar con `set -x`:
```Bash
+ echo 'Iniciando depuración'
Iniciando depuración
+ my_var='¡Hola, mundo de la depuración!'
+ echo '¡Hola, mundo de la depuración!'
¡Hola, mundo de la depuración!
+ set +x
```

## Profundización
Históricamente, depurar scripts de Bash significaba llenar tu código con declaraciones `echo`. Pero luego llegó `set -x`, que nos ofrece una vista de la ejecución en tiempo real sin impresiones manuales. Y para aquellos que desean más control, apareció el depurador `bashdb`, inspirado en el depurador gdb para C/C++.

En cuanto a alternativas, más allá de los comandos `set` (`-x`, `-v`, `-e`), otras opciones incluyen redireccionar la salida a un archivo para su análisis o usar herramientas externas como ShellCheck para análisis estático.

En términos de implementación, `set -x` es fácil; es una opción nativa de Bash que imprime comandos y sus argumentos a medida que se ejecutan. `bashdb`, por otro lado, permite avanzar paso a paso por el código, establecer puntos de interrupción y evaluar expresiones, cosas que te dan una oportunidad de lucha contra errores más escurridizos.

## Ver también
- Proyecto Depurador Bash: http://bashdb.sourceforge.net/
- "Pro Bash Programming" por Chris Johnson y Jayant Varma para scripting avanzado.
- ShellCheck para análisis estático: https://www.shellcheck.net/
