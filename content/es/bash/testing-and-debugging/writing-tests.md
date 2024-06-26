---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:26.468596-07:00
description: "C\xF3mo hacerlo: Bash no tiene un marco de pruebas integrado, pero puedes\
  \ escribir funciones de prueba simples. Para pruebas m\xE1s sofisticadas, herramientas\u2026"
lastmod: '2024-03-13T22:44:59.250142-06:00'
model: gpt-4-0125-preview
summary: Bash no tiene un marco de pruebas integrado, pero puedes escribir funciones
  de prueba simples.
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:
Bash no tiene un marco de pruebas integrado, pero puedes escribir funciones de prueba simples. Para pruebas más sofisticadas, herramientas de terceros como `bats-core` son populares.

### Ejemplo Básico de Prueba en Bash Puro:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Prueba superada."
    return 0
  else
    echo "Prueba fallida. Esperaba '$expected_output', obtuve '$result'"
    return 1
  fi
}

# Invocando la función de prueba
test_example_function
```
Salida de Muestra:
```
Prueba superada.
```

### Usando `bats-core` para Pruebas:
Primero, instala `bats-core`. Esto normalmente se puede hacer a través de tu gestor de paquetes o clonando su repositorio.

Luego, escribe tus pruebas en archivos `.bats` separados.

```bash
# Archivo: example_function.bats

#!/usr/bin/env bats

@test "probar la función de ejemplo" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Para ejecutar tus pruebas, simplemente ejecuta el archivo `.bats`:
```bash
bats example_function.bats
```
Salida de Muestra:
```
 ✓ probar la función de ejemplo

1 prueba, 0 fallos
```

Este enfoque te permite integrar fácilmente las pruebas en tu flujo de trabajo de desarrollo, asegurando la fiabilidad y estabilidad de tus scripts de Bash.
