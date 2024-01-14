---
title:    "Fish Shell: Capitalizando una cadena"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador que trabaja con el Fish Shell, es posible que necesites capitalizar cadenas de texto en tus programas. Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula. Por ejemplo, "hola mundo" se convertiría en "Hola Mundo". Esta es una práctica común en programación y puede ser útil en diferentes situaciones.

## Cómo hacerlo

Para capitalizar una cadena en el Fish Shell, puedes utilizar el siguiente comando:

```Fish Shell
echo "hola mundo" | awk '{ print toupper(substr($0, 1, 1)) substr($0, 2) }'
```

La salida de este comando será "Hola Mundo". Explicando el comando, primero tomamos la cadena "hola mundo" y la pasamos a la función `awk`. Luego, utilizamos la función `substr` para tomar la primera letra de la cadena y convertirla a mayúscula con la función `toupper`. Finalmente, utilizamos la función `substr` nuevamente para tomar el resto de la cadena (sin la primera letra) y la dejamos en minúscula. De esta manera, obtenemos la cadena final con la primera letra de cada palabra en mayúscula.

También puedes utilizar el comando `awk` de la siguiente manera:

```Fish Shell
echo "hola mundo" | awk '{print toupper($1) " " toupper($2)}'
```

En este caso, estamos capitalizando cada palabra por separado y luego uniendo las dos palabras con un espacio entre ellas. La salida también será "Hola Mundo", pero este método puede ser útil si necesitas capitalizar más de dos palabras.

## Profundizando

Una cosa a tener en cuenta es que estos comandos sólo capitalizan la primera letra de cada palabra en minúscula. Si la palabra ya comienza con una letra mayúscula, se mantendrá igual. Por ejemplo, "Hola mundo" seguirá siendo "Hola mundo" después de ser capitalizada.

Además, si quieres que todas las letras de la cadena se conviertan a mayúscula, puedes utilizar la función `toupper` sin especificar una posición de la cadena:

```Fish Shell
echo "hola mundo" | awk '{ print toupper($0) }'
```

La salida en este caso sería "HOLA MUNDO".

Ten en cuenta que estas soluciones son específicas para el Fish Shell. Si estás utilizando otro shell, es posible que necesites utilizar un comando diferente o una librería específica para capitalizar cadenas.

## Véase también

- [Página oficial de Fish Shell](https://fishshell.com/)
- [Documentación oficial de awk](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Tutorial de programación en el Fish Shell](https://fishshell.com/docs/current/tutorial.html)