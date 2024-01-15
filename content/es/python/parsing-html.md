---
title:                "Analizando HTML"
html_title:           "Python: Analizando HTML"
simple_title:         "Analizando HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en el desarrollo web o en la extracción de datos de sitios web, es probable que en algún momento necesites analizar y extraer información de páginas HTML. El proceso de análisis de HTML, también conocido como "parsing", es una habilidad valiosa que puede ayudarte a automatizar tareas y obtener datos importantes de manera rápida y eficiente.

## Cómo hacerlo

Para analizar HTML en Python, necesitarás utilizar una librería llamada "BeautifulSoup". Esta librería te permite navegar y extraer datos de una página HTML de manera sencilla.

Primero, debes importar la librería en tu código:

```Python
from bs4 import BeautifulSoup
```

Luego, puedes utilizar la función "BeautifulSoup" para crear un objeto a partir del HTML de una página web. Por ejemplo, si queremos analizar la página de Wikipedia en español, podemos hacer lo siguiente:

```Python
#Obtenemos el HTML de la página
pagina = urllib.urlopen("https://es.wikipedia.org/wiki/P%C3%A1gina_principal")

#Creamos un objeto BeautifulSoup
soup = BeautifulSoup(pagina.read(), 'html.parser')
```

Una vez que tengas el objeto creado, puedes utilizar diferentes métodos para navegar y extraer información específica de la página. Por ejemplo, si queremos obtener el título de la página y guardarlo en una variable, podemos hacer lo siguiente:

```Python
titulo = soup.title.string
```

Además de obtener información, también puedes utilizar BeautifulSoup para buscar y filtrar elementos específicos en el HTML. Por ejemplo, si queremos obtener todos los enlaces de la página, podemos utilizar el método "find_all" y especificar el tipo de elemento que estamos buscando:

```Python
enlaces = soup.find_all('a')
```

## Deep Dive

Además de los métodos mencionados anteriormente, BeautifulSoup también permite utilizar selectores CSS para seleccionar elementos específicos en la página. Esto puede ser útil cuando queremos extraer información de una sección específica del HTML.

Por ejemplo, si queremos obtener los nombres de todos los elementos de una lista, podemos utilizar un selector CSS para seleccionar todos los elementos de la lista y luego iterar sobre ellos para obtener sus nombres. Utilizar selectores CSS en BeautifulSoup es muy similar a como lo haríamos en una hoja de estilos CSS. Por ejemplo:

```Python
#Seleccionamos todos los elementos "li" dentro de una lista desordenada
items = soup.select('ul li')

#Iteramos sobre cada elemento para obtener su nombre
for item in items:
    nombre = item.text
```

También podemos utilizar selectores CSS para buscar elementos por clase o id. Por ejemplo, si queremos obtener todos los elementos con la clase "titulo" en la página, podemos hacer lo siguiente:

```Python
titulos = soup.select('.titulo')
```

Si quieres aprender más sobre BeautifulSoup y todas sus funcionalidades, puedes consultar su documentación oficial [aquí](https://www.crummy.com/software/BeautifulSoup/bs4/doc/).

## Véase también

- [Documentación oficial de BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Tutorial de BeautifulSoup en español](https://www.aprendemachinelearning.com/beautifulsoup-en-espanol/)
- [Cursos de Python en Codecademy](https://www.codecademy.com/learn/learn-python)