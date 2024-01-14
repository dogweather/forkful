---
title:    "Bash: Buscando y reemplazando texto"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Bash, es muy probable que hayas tenido que hacer tareas repetitivas como buscar y reemplazar texto en tus scripts. Engancharse en este tipo de actividades puede ser tedioso y consumir mucho tiempo, pero ¡no temas! Bash tiene una solución rápida y sencilla para este problema. En este artículo, aprenderás cómo puedes utilizar el comando `sed` para realizar búsqueda y reemplazo de texto de manera eficiente en tus programas Bash.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo con `sed`, sigue los siguientes pasos:

1. Comienza abriendo tu terminal y navegando a la ubicación donde se encuentre tu script Bash.
2. Una vez allí, ejecuta el comando `sed` seguido de una expresión entre comillas, como en el siguiente ejemplo:

```Bash
sed 's/old_text/new_text/' myscript.sh
```
En este ejemplo, reemplazaremos el texto "old_text" por "new_text" en el archivo "myscript.sh".
3. Puedes especificar el archivo donde deseas realizar el reemplazo o, si prefieres, puedes usar `sed` directamente en la terminal, sin necesidad de un archivo.

```Bash
echo "Soy un texto original" | sed 's/original/nuevo/'
```

4. Puedes reemplazar todo el texto que coincida con `s/busqueda/texto_ingresado/` o solo la primera coincidencia con `s/busqueda/texto_ingresado/1`.
5. Si deseas realizar el reemplazo sin considerar mayúsculas o minúsculas, agrega la letra `i` al final, como en el siguiente ejemplo:

```Bash
sed 's/busqueda/texto_ingresado/i'
```

## Inmersión Profunda
Ahora que ya sabes cómo utilizar `sed` para realizar búsqueda y reemplazo de texto, profundicemos un poco más en este comando. A continuación, algunos consejos que pueden ser útiles:

- Puedes usar la combinación de comandos `sed` y `grep` para buscar y reemplazar solo en archivos que coincidan con un patrón específico.
- Si deseas realizar un reemplazo recursivo en varios archivos a la vez, utiliza el comando `find` junto con `sed`, como en el siguiente ejemplo:
```Bash
find . -name "*.txt" -exec sed -i 's/busqueda/texto_ingresado/g' {} \;
```
- `sed` también permite realizar reemplazos utilizando expresiones regulares, lo que puede ser útil para realizar cambios en texto que siga un patrón específico.

¡Ahora está listo para utilizar `sed` de manera efectiva en tus scripts Bash!

## Ver también
- [Documentación oficial de `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial sobre Expresiones Regulares en `sed`](https://www.youtube.com/watch?v=Lnfi-fvMftU)
- [Otros comandos útiles de Bash](https://linuxize.com/post/bash-check-if-file-exists/)

¡Espero que este artículo te haya sido útil y te ayude a ser más eficiente en tus tareas de búsqueda y reemplazo de texto en Bash! ¡Hasta la próxima!