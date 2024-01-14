---
title:    "Fish Shell: Eliminando caracteres que coinciden con un patrón"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

Si eres programador y utilizas el Fish Shell, es posible que alguna vez te hayas encontrado con la tarea de eliminar caracteres que coinciden con un patrón determinado. Ya sea porque necesitas limpiar datos o por motivos de seguridad, la eliminación de caracteres en un archivo o texto puede ser una tarea útil. En este artículo, veremos cómo llevar a cabo esta tarea de forma sencilla utilizando Fish Shell.

## Cómo hacerlo

Primeramente, debemos abrir una terminal y acceder al Fish Shell. Una vez allí, podemos utilizar el comando "sed" seguido del patrón que deseamos buscar y el archivo o texto donde realizar la búsqueda. Por ejemplo:

```Fish Shell
sed 's/patrón//' archivo.txt
```

Esto eliminará todas las ocurrencias del patrón en el archivo "archivo.txt". Si deseamos eliminar solamente la primera coincidencia, podemos utilizar el flag "-n" seguido del comando "p" (impresión), de la siguiente manera:

```Fish Shell
sed -n 's/pattern//' archivo.txt
```

También es posible utilizar un patrón más complejo para buscar y eliminar caracteres. Por ejemplo, si queremos borrar todos los números en un texto, podemos utilizar el siguiente comando:

```Fish Shell
sed 's/[0-9]//g' archivo.txt
```

Este comando busca todas las ocurrencias de números (0-9) y las elimina. El flag "g" significa que se aplicará a todas las coincidencias en cada línea del archivo.

## Profundizando

"sed" es una herramienta muy útil para realizar cambios en archivos o textos mediante patrones. Además de eliminar caracteres, también podemos utilizarlo para reemplazar caracteres o agregar texto en lugares específicos. Para conocer más opciones y comandos avanzados, puedes consultar la documentación oficial de Fish Shell o buscar tutoriales en línea.

## Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de sed en línea de comandos](http://linuxcommand.org/lc3_adv_sed.php)
- [Tutorial avanzado de sed por Andrea Aime](https://aime2-j average.blogspot.com/2012/01/sed-basics-and-more.html)