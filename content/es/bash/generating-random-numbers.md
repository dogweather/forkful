---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:37.038987-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Generar números aleatorios es crear valores que no se pueden predecir razonablemente antes de ser generados. Los programadores los usan para todo, desde simulaciones y pruebas hasta juegos y seguridad informática.

## Cómo:
Para generar un número aleatorio en Bash:

```Bash
$ echo $((RANDOM%100))  # Genera un número entre 0 y 99
```

Muestra de salida:

```Bash
$ 23
```

Para un rango específico, por ejemplo, entre 20 y 50:

```Bash
$ echo $((RANDOM % (50 - 20 + 1) + 20))
```

Muestra de salida:

```Bash
$ 29
```

## Inmersión Profunda
La generación de números aleatorios en computadoras comenzó en la década de 1940. Al principio, los números eran precalculados y almacenados en tablas. Hoy, los programas utilizan algoritmos para generar números pseudoaleatorios; 'pseudo' porque la completa aleatoriedad es imposible con algoritmos deterministas.

Alternativas en Bash pueden incluir `openssl` para criptografía segura o `/dev/random` y `/dev/urandom` para obtener aleatoriedad que proviene del ruido del sistema.

Detalles de implementación: `RANDOM` en Bash es una variable interna que proporciona números pseudoaleatorios. No es adecuada para criptografía debido a su predecibilidad en comparación con herramientas especializadas.

## Ver También
- "man rand" - para obtener más detalles sobre la aleatoriedad en sistemas Unix.
- "man openssl" - información sobre cómo utilizar `openssl` para generar números aleatorios.
- [RANDOM: Bash's Random Number Generator](https://tldp.org/LDP/abs/html/randomvar.html) - detalles sobre la variable `RANDOM` en Bash.

Recuerda, cada uso tiene su herramienta adecuada, ¡así que elige sabiamente!