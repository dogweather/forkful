---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:11.809657-07:00
description: "C\xF3mo hacerlo: Fish Shell utiliza el comando `test` para verificar\
  \ tipos de archivos y caracter\xEDsticas, incluyendo si un objetivo es un directorio.\
  \ Aqu\xED\u2026"
lastmod: '2024-03-13T22:44:59.515715-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell utiliza el comando `test` para verificar tipos de archivos y\
  \ caracter\xEDsticas, incluyendo si un objetivo es un directorio."
title: Comprobando si un directorio existe
weight: 20
---

## Cómo hacerlo:
Fish Shell utiliza el comando `test` para verificar tipos de archivos y características, incluyendo si un objetivo es un directorio. Aquí hay un patrón básico para verificar si un directorio existe:

```fish
if test -d /ruta/al/dir
    echo "El directorio existe"
else
    echo "El directorio no existe"
end
```
Salida de muestra:
```
El directorio existe
```

Para operaciones de archivos y directorios más simplificadas, uno podría recurrir a herramientas externas como `fd`, aunque se usa más comúnmente para encontrar archivos y directorios en lugar de simplemente verificar su existencia. Sin embargo, combinarlo con scripts de Fish puede producir resultados útiles:

```fish
set dir "/ruta/a/buscar"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "El directorio existe"
else
    echo "El directorio no existe"
end
```

Este ejemplo de `fd` busca el directorio a una profundidad especificada, y `grep` verifica la coincidencia, haciéndolo versátil para comprobaciones matizadas. Sin embargo, para el propósito directo de verificar la existencia, apegarse al `test` incorporado de Fish es tanto eficiente como sencillo.
