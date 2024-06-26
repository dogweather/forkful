---
date: 2024-01-26 00:51:37.323383-07:00
description: "C\xF3mo hacerlo: Para atrapar errores en Fish, ap\xF3yate en el comando\
  \ `status` y las condicionales. Supongamos que `ping` falla; aqu\xED te mostramos\
  \ c\xF3mo\u2026"
lastmod: '2024-03-13T22:44:59.509128-06:00'
model: gpt-4-1106-preview
summary: "Para atrapar errores en Fish, ap\xF3yate en el comando `status` y las condicionales."
title: Manejo de errores
weight: 16
---

## Cómo hacerlo:
Para atrapar errores en Fish, apóyate en el comando `status` y las condicionales. Supongamos que `ping` falla; aquí te mostramos cómo detectarlo:

```fish
ping -c 1 example.com
if not status is-success
    echo "Algo raro pasó con el ping."
end
```

Salida de muestra si `ping` falla:

```
Algo raro pasó con el ping.
```

Para manejar un código de error específico, usa `status --is`:

```fish
false
if status --is 1
    echo "Se capturó un error con código 1."
end
```

Salida de muestra:
```
Se capturó un error con código 1.
```

Para un enfoque más robusto, considera usar una función:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping falló con estado $status"
        return 1
    end
end

try_ping
```

## Análisis Profundo
El manejo de errores en Fish no coincide con el paradigma `try/catch` que podrías conocer de los lenguajes de alto nivel. En cambio, tienes estados de salida directos proporcionados por el comando `status`.

Históricamente, en los sistemas tipo Unix, un estado de salida de `0` significa éxito, mientras que cualquier valor distinto de cero indica un error, lo cual comúnmente refleja diferentes razones de fallo. Esta convención es empleada por la mayoría de las utilidades de línea de comandos y por ende, por el propio Fish.

Las alternativas a las verificaciones de `status` en Fish incluyen el manejo de señales a través de `trap` en otros shells, pero Fish prefiere una verificación de estado más explícita, ya que es más limpia y menos propensa a efectos secundarios.

En cuanto a la implementación, el manejo de errores en Fish sigue siendo sencillo pero poderoso, en gran medida debido a su naturaleza no bloqueante y énfasis en la sintaxis clara, como se muestra en los ejemplos. Los códigos de error se integran bien con las funciones, permitiendo una gestión de errores modular y legible.

## Ver También
- Documentación de Fish sobre condicionales: https://fishshell.com/docs/current/language.html#conditionals
- Tutorial de Fish sobre manejo de errores: https://fishshell.com/docs/current/tutorial.html#error-handling
