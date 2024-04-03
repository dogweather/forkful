---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:36.411388-07:00
description: "C\xF3mo: Trabajar directamente con YAML en Bash requiere un poco de\
  \ ingenio ya que Bash no tiene soporte incorporado para analizar YAML. Sin embargo,\
  \ puedes\u2026"
lastmod: '2024-03-13T22:44:59.267727-06:00'
model: gpt-4-0125-preview
summary: Trabajar directamente con YAML en Bash requiere un poco de ingenio ya que
  Bash no tiene soporte incorporado para analizar YAML.
title: Trabajando con YAML
weight: 41
---

## Cómo:
Trabajar directamente con YAML en Bash requiere un poco de ingenio ya que Bash no tiene soporte incorporado para analizar YAML. Sin embargo, puedes usar herramientas externas como `yq` (un procesador de YAML de línea de comandos ligero y portátil) para interactuar de manera eficiente con archivos YAML. Repasemos algunas operaciones comunes:

### Instalando `yq`:
Antes de sumergirte en los ejemplos, asegúrate de tener `yq` instalado. Usualmente puedes instalarlo desde tu gestor de paquetes, por ejemplo, en Ubuntu:

```bash
sudo apt-get install yq
```

O puedes descargarlo directamente de su repositorio de GitHub.

### Leyendo un valor:
Considera que tienes un archivo llamado `config.yaml` con el siguiente contenido:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secreto
```

Para leer el host de la base de datos, puedes usar `yq` de la siguiente manera:

```bash
yq e '.database.host' config.yaml
```

**Salida de muestra:**

```
localhost
```

### Actualizando un valor:
Para actualizar el nombre del usuario en `config.yaml`, usa el comando `yq eval` con la opción `-i` (en el lugar):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Verifica el cambio con:

```bash
yq e '.user.name' config.yaml
```

**Salida de muestra:**

```
newadmin
```

### Agregando un nuevo elemento:
Para añadir un nuevo elemento bajo la sección de base de datos, como un nuevo campo `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Revisar el contenido del archivo confirmará la adición.

### Eliminando un elemento:
Para eliminar la contraseña bajo usuario:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Esta operación eliminará el campo de contraseña de la configuración.

Recuerda, `yq` es una herramienta poderosa y tiene muchas más capacidades, incluyendo la conversión de YAML a JSON, la fusión de archivos, y manipulaciones aún más complejas. Consulta la documentación de `yq` para explorar más a fondo.
