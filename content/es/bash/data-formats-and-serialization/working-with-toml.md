---
date: 2024-01-26 04:18:39.555054-07:00
description: "TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato\
  \ de serializaci\xF3n de datos. A los programadores les gusta por su simplicidad\
  \ y\u2026"
lastmod: '2024-03-13T22:44:59.271269-06:00'
model: gpt-4-0125-preview
summary: "TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato de serializaci\xF3\
  n de datos. A los programadores les gusta por su simplicidad y\u2026"
title: Trabajando con TOML
weight: 39
---

## Qué y por qué?
TOML, abreviatura de Tom's Obvious, Minimal Language, es un formato de serialización de datos. A los programadores les gusta por su simplicidad y legibilidad; es primo para archivos de configuración, con vibras similares a YAML pero menos engorroso que JSON para un humano.

## Cómo hacerlo:
Primero, instala `toml-cli` para jugar con TOML en Bash. Útil para leer o editar archivos TOML al vuelo.

```Bash
# Instala toml-cli, nuestro pequeño ayudante para tareas TOML
pip install toml-cli

# Imagina que tienes un archivo TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Leer un valor
toml get config.toml owner.name
# Salida: Tom

# Establecer un valor
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Consejo profesional: ¡Usa comillas para claves con puntos o caracteres raros!
```

## Profundización
Nacido del desagrado por los obstáculos de JSON para los humanos, TOML apareció alrededor de 2013. Tom Preston-Werner, cofundador de GitHub, quería algo súper legible. YAML e INI eran alternativas pero TOML es como lo mejor de ambos.

¡Pum! Tienes datos anidados y matrices, menos las trampas de YAML y las llaves rizadas de JSON. TOML ahora es una opción para la configuración en Cargo de Rust, lo que habla de su ascenso en el mundo del desarrollo. Está impulsado por una especificación, manteniendo las cosas ajustadas y bien definidas. Conseguirás analizadores en casi cualquier idioma, lo que lo hace ampliamente adoptable.

## Ver también
- Repositorio oficial de TOML en GitHub: https://github.com/toml-lang/toml
- toml-cli en PyPI: https://pypi.org/project/toml-cli/
- Comparación de formatos de serialización de datos: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
