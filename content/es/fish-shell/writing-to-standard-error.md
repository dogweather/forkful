---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Escribir en el error estándar permite separar la salida normal de los mensajes de error. Los programadores lo usan para depurar y proporcionar información de diagnóstico sin afectar la salida principal del programa.

## Cómo hacerlo:

```Fish Shell
# Enviar un mensaje de error a stderr
echo "¡Ha ocurrido un error!" >&2

# Ejemplo de redireccionamiento de stderr a un archivo
echo "¡Ha ocurrido un error!" 2> error_log.txt

# Ejemplo combinando stdout y stderr en un solo archivo
echo "Este es un mensaje normal"
echo "Este es un mensaje de error" >&2
both_output.txt 2>&1
```

Salida de muestra de stderr en la consola:

```
¡Ha ocurrido un error!
```

Contenido de `error_log.txt` tras la redirección:

```
¡Ha ocurrido un error!
```

## Análisis a fondo:

Históricamente, las terminales proporcionaban diversas corrientes para distintos tipos de información. Al estándar de error (stderr) le asignaron el descriptor de archivo 2. Usarlo garantiza que, incluso si la salida estándar (stdout) se redirecciona, los errores siguen siendo visibles o manejables de forma separada.

Alternativas incluyen el uso de syslog en sistemas Unix-like para manejar mensajes de error, aunque suele utilizarse para programas que corren como servicios o daemons.

Detalles de implementación: en Fish Shell, al igual que en otros shells, se usa el operador '>&' para redireccionar un descriptor de archivo específico. Es crucial para el diseño de scripts y programas que sean robustos y fáciles de mantener.

## Ver también:

- Documentación oficial de Fish Shell sobre la redirección de salida: [fishshell.com/docs/current/index.html#redirects](https://fishshell.com/docs/current/index.html#redirects)
- Tutorial POSIX Shell acerca de redirección de I/O: [https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html)
- Explicación en Stack Overflow de stderr: [https://stackoverflow.com/questions/3385201/confused-about-stdin-stdout-and-stderr](https://stackoverflow.com/questions/3385201/confused-about-stdin-stdout-and-stderr)
