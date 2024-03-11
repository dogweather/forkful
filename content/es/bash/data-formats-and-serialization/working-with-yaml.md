---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:36.411388-07:00
description: "YAML, que significa YAML Ain't Markup Language (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos legible por humanos\
  \ que se\u2026"
lastmod: '2024-03-11T00:14:33.087784-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa YAML Ain't Markup Language (YAML no es un lenguaje de\
  \ marcado), es un est\xE1ndar de serializaci\xF3n de datos legible por humanos que\
  \ se\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

YAML, que significa YAML Ain't Markup Language (YAML no es un lenguaje de marcado), es un estándar de serialización de datos legible por humanos que se puede utilizar para archivos de configuración, así como en aplicaciones donde se almacenan o transmiten datos. Los programadores se inclinan hacia YAML debido a su claridad y simplicidad, especialmente en proyectos que involucran configuraciones complejas o la necesidad de estructuras de datos fácilmente editables.

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
