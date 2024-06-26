---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:23.602298-07:00
description: "C\xF3mo hacerlo: Fish Shell no tiene soporte integrado para analizar\
  \ YAML, pero puedes utilizar herramientas de terceros como `yq` (un procesador de\
  \ YAML de\u2026"
lastmod: '2024-03-13T22:44:59.521602-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell no tiene soporte integrado para analizar YAML, pero puedes utilizar\
  \ herramientas de terceros como `yq` (un procesador de YAML de l\xEDnea de comandos\
  \ ligero y port\xE1til) para manejar datos YAML."
title: Trabajando con YAML
weight: 41
---

## Cómo hacerlo:
Fish Shell no tiene soporte integrado para analizar YAML, pero puedes utilizar herramientas de terceros como `yq` (un procesador de YAML de línea de comandos ligero y portátil) para manejar datos YAML.

**Instalación de yq (si aún no está instalado):**
```fish
sudo apt-get install yq
```

**Leyendo un valor de un archivo YAML:**
Supongamos que tienes un archivo YAML `config.yaml` con el siguiente contenido:
```yaml
database:
  host: localhost
  port: 3306
```

Para leer el host de la base de datos, usarías:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Salida de ejemplo:**
```
localhost
```

**Actualizando un valor en un archivo YAML:**
Para actualizar el `puerto` a `5432`, usa:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verificar la actualización:**
```fish
yq e '.database.port' config.yaml
```
**Salida de ejemplo:**
```
5432
```

**Escribiendo un nuevo archivo YAML:**
Para crear un nuevo `new_config.yaml` con contenido predefinido:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Esto utiliza `yq` para procesar e imprimir de forma bonita (-P flag) una cadena en un nuevo archivo YAML.

**Analizando estructuras complejas:**
Si tienes un archivo YAML más complejo y necesitas buscar matrices o objetos anidados, puedes:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Salida de ejemplo:**
```
server1
server2
```
Utilizando `yq`, Fish Shell hace que sea sencillo navegar a través de documentos YAML y manipularlos para diversas tareas de automatización y configuración.
