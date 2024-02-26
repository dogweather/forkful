---
date: 2024-01-26 04:21:19.944616-07:00
description: "TOML es un formato de archivo de configuraci\xF3n, f\xE1cil de leer\
  \ y escribir para los humanos, y f\xE1cil de analizar y generar para las m\xE1quinas.\
  \ Los\u2026"
lastmod: '2024-02-25T18:49:55.993946-07:00'
model: gpt-4-0125-preview
summary: "TOML es un formato de archivo de configuraci\xF3n, f\xE1cil de leer y escribir\
  \ para los humanos, y f\xE1cil de analizar y generar para las m\xE1quinas. Los\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
TOML es un formato de archivo de configuración, fácil de leer y escribir para los humanos, y fácil de analizar y generar para las máquinas. Los programadores trabajan con TOML para archivos de configuración claros y jerárquicos en proyectos donde la legibilidad es clave.

## Cómo hacerlo:
Para leer y manipular TOML en Fish, podrías usar una herramienta como `yj`, que puede convertir TOML a JSON. Aquí te mostramos cómo:

```fish
# Instalar yj vía Fisher
fisher install jorgebucaran/yj

# Convertir TOML a JSON
echo 'title = "Ejemplo TOML"' | yj -tj

# Salida de muestra
{"title":"Ejemplo TOML"}
```

Para escribir en TOML, inviertes el proceso:

```fish
# Convertir JSON a TOML
echo '{"title":"Ejemplo JSON"}' | yj -jt

# Salida de muestra
title = "Ejemplo JSON"
```

Para tareas más complejas, considera una herramienta CLI de TOML dedicada como `toml-cli`.

```fish
# Instalar toml-cli
pip install toml-cli

# Establecer un valor en archivo TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Obtener un valor del archivo TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Estudio Profundo
TOML (Tom's Obvious, Minimal Language), introducido por Tom Preston-Werner en 2013, es similar a INI pero con una especificación definida y jerarquía de datos. JSON y YAML son las principales alternativas, pero tienen sus compromisos: JSON no es tan amigable para los humanos, mientras que YAML es más complejo. El diseño de TOML prospera en escenarios donde los archivos de configuración son a menudo mantenidos a mano, equilibrando la simplicidad y expresividad. Cuando se trata de implementación, hay analizadores de TOML disponibles para la mayoría de los lenguajes de programación, incluido TomlBombadil para Fish que se puede integrar directamente en tus scripts.

## Ver También
- Especificación oficial de TOML: https://toml.io
- `yj`, una herramienta para convertir entre TOML, JSON, YAML y XML: https://github.com/jorgebucaran/yj
- `toml-cli`, una utilidad de línea de comandos para TOML: https://github.com/sdispater/toml-cli
