---
aliases:
- /es/elixir/rounding-numbers/
date: 2024-01-26 03:43:42.156475-07:00
description: "Redondear n\xFAmeros significa ajustarlos a un valor cercano por simplicidad\
  \ o para coincidir con cierta precisi\xF3n. Es \xFAtil para mejorar la legibilidad,\u2026"
lastmod: 2024-02-18 23:09:09.643018
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros significa ajustarlos a un valor cercano por simplicidad\
  \ o para coincidir con cierta precisi\xF3n. Es \xFAtil para mejorar la legibilidad,\u2026"
title: "Redondeo de n\xFAmeros"
---

{{< edit_this_page >}}

## Qué y por qué?
Redondear números significa ajustarlos a un valor cercano por simplicidad o para coincidir con cierta precisión. Es útil para mejorar la legibilidad, reducir el espacio de almacenamiento o satisfacer necesidades específicas del dominio, como cálculos monetarios donde quieres redondear al centavo más cercano.

## Cómo hacerlo:
En Elixir, puedes usar `Float.round/2` para redondear un número de punto flotante. Puedes especificar el número de dígitos decimales que deseas mantener. Así es como funciona:

```elixir
# Redondear un número a ningún decimal
Float.round(3.14159) # => 3.0

# Redondear un número a 2 decimales
Float.round(3.14159, 2) # => 3.14

# Redondear un número con precisión negativa al 10 más cercano
Float.round(123.456, -1) # => 120.0
```

## Estudio Detallado
Redondear números es un problema clásico en informática, tanto es así que la elección de la estrategia de redondeo puede impactar en sistemas financieros, cálculos científicos y más. `Float.round/2` de Elixir por defecto utiliza el redondeo "hacia arriba", parecido al redondeo tradicional enseñado en clase de matemáticas.

Si necesitas otros tipos de redondeo, Elixir te permite crear el tuyo propio. Considera, por ejemplo, el redondeo "hacia abajo" (siempre hacia abajo) o el redondeo "hacia arriba" (siempre hacia arriba). Usarías `Float.floor/1` o `Float.ceil/1`, respectivamente.

```elixir
# Redondeo hacia abajo
Float.floor(3.999) # => 3.0

# Redondeo hacia arriba
Float.ceil(3.001) # => 4.0
```

Estas alternativas ayudan a personalizar el redondeo según las necesidades exactas de tu aplicación, ya sea cálculos financieros, renderización de gráficos o aproximación de datos.

## Ver También
Para más información sobre las funciones de redondeo de Elixir y números de punto flotante:

- Documentación oficial de Elixir sobre `Float`: https://hexdocs.pm/elixir/Float.html
- Estándar IEEE para Aritmética de Punto Flotante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
