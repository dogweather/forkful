---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:24.789891-07:00
description: "C\xF3mo: Fish no tiene un marco de pruebas integrado como algunos otros\
  \ entornos de programaci\xF3n. Sin embargo, puedes escribir scripts de pruebas simples\
  \ que\u2026"
lastmod: '2024-03-13T22:44:59.505155-06:00'
model: gpt-4-0125-preview
summary: "Fish no tiene un marco de pruebas integrado como algunos otros entornos\
  \ de programaci\xF3n."
title: Escribiendo pruebas
weight: 36
---

## Cómo:
Fish no tiene un marco de pruebas integrado como algunos otros entornos de programación. Sin embargo, puedes escribir scripts de pruebas simples que usen afirmaciones para verificar el comportamiento de tus funciones. Adicionalmente, puedes aprovechar herramientas de terceros como `fishtape` para un conjunto de pruebas más completo.

### Ejemplo 1: Script de Prueba Básico
Comencemos con una función básica en Fish que calcula la suma de dos números:

```fish
function add --description 'Suma dos números'
    set -l suma (math $argv[1] + $argv[2])
    echo $suma
end
```

Puedes escribir un script de prueba básico para esta función de la siguiente manera:

```fish
function test_add
    set -l resultado (add 3 4)
    if test $resultado -eq 7
        echo "test_add aprobado"
    else
        echo "test_add falló"
    end
end

test_add
```

Al ejecutar este script, se mostraría:

```
test_add aprobado
```

### Ejemplo 2: Usando Fishtape
Para una solución de pruebas más robusta, puedes usar `fishtape`, un corredor de pruebas que produce TAP para Fish.

Primero, instala `fishtape` si aún no lo has hecho:

```fish
fisher install jorgebucaran/fishtape
```

Luego, crea un archivo de prueba para tu función `add`, por ejemplo, `add_test.fish`:

```fish
test "Sumar 3 y 4 da como resultado 7"
    set resultado (add 3 4)
    echo "$resultado" | fishtape
end
```

Para ejecutar la prueba, usa el siguiente comando:

```fish
fishtape add_test.fish
```

Un ejemplo de salida sería:

```
TAP version 13
# Sumar 3 y 4 da como resultado 7
ok 1 - test_add aprobado
```

Esto te indica que la prueba se aprobó con éxito. `fishtape` te permite estructurar pruebas más detalladas y proporciona una salida informativa, facilitando la depuración y cobertura de pruebas completa para tus scripts de Fish.
