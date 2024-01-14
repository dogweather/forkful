---
title:                "Fish Shell: Búsqueda y reemplazo de texto"
simple_title:         "Búsqueda y reemplazo de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador o alguien que trabaja con texto en una computadora, seguramente habrás encontrado la necesidad de buscar y reemplazar texto en tus archivos. Esta tarea puede resultar tediosa y llevar mucho tiempo si se hace manualmente. Por suerte, el Fish Shell tiene una función incorporada que hace que esta tarea sea mucho más fácil y rápida. En este artículo, te enseñaremos cómo utilizar la función de búsqueda y reemplazo de texto en el Fish Shell.

## Cómo hacerlo

Para realizar una búsqueda y reemplazo de texto en el Fish Shell, sigue estos sencillos pasos:

1. Abre tu terminal y accede al Fish Shell.
2. Selecciona el archivo en el que deseas realizar la búsqueda y reemplazo usando el comando `cd` para navegar a la ubicación del archivo.
3. Una vez en la ubicación correcta, utiliza el comando `find` seguido del texto que deseas buscar. Por ejemplo, si deseas buscar la palabra "hola" en un archivo llamado `texto.txt`, el comando sería `find hola texto.txt`.
4. Luego, utiliza el comando `sed` seguido del texto que deseas reemplazar y nuevamente el archivo en el que deseas realizar el reemplazo. Por ejemplo, si deseas reemplazar "hola" con "hola a todos" en el archivo `texto.txt`, el comando sería `sed -i 's/hola/hola a todos/' texto.txt`.
5. ¡Listo! Ahora puedes usar el comando `cat` para verificar el archivo y confirmar que el texto ha sido reemplazado correctamente.

Un ejemplo de cómo se vería esto en la práctica:

```Fish Shell
cd carpeta/archivo.txt
find hola texto.txt
sed -i 's/hola/hola a todos/' texto.txt
cat texto.txt
```

El comando `sed` se utiliza para realizar el reemplazo de texto, mientras que el parámetro `-i` indica que se debe modificar el archivo original. Si no se utiliza este parámetro, se creará un nuevo archivo con el texto reemplazado.

## Profundizando

La función de búsqueda y reemplazo en el Fish Shell utiliza la herramienta `sed`, que es una poderosa utilidad de Linux para realizar ediciones en archivos de texto de forma automática. Con el comando `find` se busca el texto especificado y con el comando `sed` se reemplaza el texto encontrado.

Es importante tener en cuenta que en el uso de este comando, el texto buscado y el texto de reemplazo deben estar entre comillas simples (`'`) para que se realice de manera adecuada.

## Ver también

- [Manual de Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de sed en Linux](https://www.unixtutorial.org/sed-find-and-replace-text)
- [Comandos básicos de Linux](https://www.hostinger.es/tutoriales/comandos-linux/)