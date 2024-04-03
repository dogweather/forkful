---
date: 2024-01-20 17:56:08.686519-07:00
description: "Leer argumentos de la l\xEDnea de comandos permite a los scripts recibir\
  \ datos externos al ser ejecutados. Los programadores hacen esto para personalizar\
  \ la\u2026"
lastmod: '2024-03-13T22:44:59.516701-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos permite a los scripts recibir\
  \ datos externos al ser ejecutados."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Qué y Por Qué?
Leer argumentos de la línea de comandos permite a los scripts recibir datos externos al ser ejecutados. Los programadores hacen esto para personalizar la ejecución según necesidades diferentes sin cambiar el código.

## Cómo:
Ejemplo sencillo:

```Fish Shell
for arg in $argv
    echo "Argumento: $arg"
end
```

Si corres este script con `fish mi_script.fish uno dos tres`, obtendrás:

```
Argumento: uno
Argumento: dos
Argumento: tres
```

Ejemplo con argumentos nombrados:

```Fish Shell
set -l usuario ""
set -l contraseña ""

for arg in $argv
    switch $arg
        case -u --usuario
            set usuario $argv[(math (status current-command-index) + 1)]
        case -p --contraseña
            set contraseña $argv[(math (status current-command-index) + 1)]
        case '*'
            echo "Opción desconocida: $arg"
            exit 1
    end
end

echo "Usuario: $usuario"
echo "Contraseña: $contraseña"
```

Si corres este script con `fish mi_script2.fish --usuario Ana --contraseña secreto`, obtendrás:

```
Usuario: Ana
Contraseña: secreto
```

## Profundización:
Originalmente, leer argumentos de la línea de comandos era esencial para scripts en sistemas Unix, lo que permitía mayor versatilidad y reutilización. 

En Fish, `$argv` es una variable que contiene los argumentos de línea de comandos como una lista. Es distinto de Bash, donde los argumentos son `$1`, `$2`, etc. 

Fish facilita iterar argumentos con un bucle `for` simple. También tiene un `switch` para manejar opciones complejas, algo parecido a `getopts` en otros shells, pero más legible.

Otras alternativas para organizar argumentos incluyen `argparse`, disponible en Fish, que es incluso más potente para scripts complicados.

## Ver También:
- [Documentación oficial de Fish](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
