---
title:    "Fish Shell: Creando un archivo temporal"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Fish Shell

Crear un archivo temporal en Fish Shell puede resultar útil en diversas situaciones. Por ejemplo, cuando estamos trabajando con datos sensibles y no queremos dejar rastros en nuestro sistema, o cuando necesitamos almacenar información temporalmente sin sobrecargar nuestro espacio de almacenamiento. En este artículo, te mostraremos cómo crear un archivo temporal en Fish Shell de manera sencilla y eficiente.

## Cómo crear un archivo temporal en Fish Shell

Crear un archivo temporal en Fish Shell es muy fácil. Solo debes seguir estos pasos:

1. Abre tu terminal y ejecuta el siguiente comando:

```Fish Shell
mktemp
```
Este comando creará un archivo temporal con un nombre aleatorio en la ubicación predeterminada. El archivo se creará vacío, pero puedes agregar contenido posteriormente.

2. Si deseas especificar la ubicación y el nombre del archivo temporal, puedes usar el siguiente comando:

```Fish Shell
mktemp -d nombre_del_archivo
```
De esta manera, el archivo se creará en la ubicación actual con el nombre especificado.

3. Una vez que hayas terminado de trabajar con el archivo temporal, asegúrate de eliminarlo del sistema para no dejar rastros ni ocupar espacio innecesariamente. Para eso, puedes usar el siguiente comando:

```Fish Shell
rm nombre_del_archivo
```

¡Y eso es todo! Has creado y eliminado un archivo temporal en Fish Shell.

## Profundizando en la creación de archivos temporales

La utilidad de crear archivos temporales en Fish Shell va más allá de simplemente almacenar datos temporales. También puedes utilizarlos para crear secuencias de comandos que te ayuden a automatizar tareas en tu sistema. Por ejemplo, puedes crear un archivo temporal con un script que realice un respaldo de tus archivos y luego eliminarlo una vez que se complete la tarea.

Otra opción es utilizar la opción "-u" para crear un archivo temporal en modo de solo escritura, lo que te permitirá proteger los datos que contiene y prevenir escrituras accidentales.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://linoxide.com/es/gnu-linux/comandos-fish-shell/)
- [Repositorio de Fish Shell en GitHub](https://github.com/fish-shell/fish-shell)