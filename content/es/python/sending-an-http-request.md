---
title:                "Enviando una solicitud http"
html_title:           "Python: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Enviar una solicitud HTTP es una forma en la que los programadores pueden comunicarse con servidores web para obtener o enviar información. Esto es especialmente útil cuando se están creando aplicaciones que interactúan con otros servicios en línea, como redes sociales o API. 

## ¿Cómo hacerlo?

Para enviar una solicitud HTTP en Python, podemos usar el módulo `requests`. Primero, debemos importarlo en nuestro código:

```python
import requests
```

Luego, podemos crear una solicitud GET a una URL específica usando la función `get()` y guardar la respuesta en una variable:

```python
response = requests.get('http://www.ejemplo.com/')
```

Podemos obtener el código de estado de la respuesta usando el atributo `status_code`:

```python
print(response.status_code)
```

También podemos ver el contenido de la respuesta usando el atributo `text`:

```python
print(response.text)
```

## Profundizando
Las solicitudes HTTP han sido una parte fundamental de la web desde su inicio en los años 90. Sin ellas, no podríamos interactuar con diferentes sitios web y servicios. Además, además del módulo `requests`, también podemos usar la biblioteca estándar `urllib` de Python para enviar solicitudes. 

Si deseamos especificar más detalles en nuestra solicitud, como encabezados personalizados o parámetros, podemos hacerlo usando los argumentos opcionales en la función `get()`. También podemos enviar solicitudes POST en lugar de GET cambiando el método en la función de solicitud. 

## Ver también
- [Documentación de la biblioteca de Python "requests"](https://docs.python-requests.org/en/master/)
- [Documentación de la biblioteca estándar de Python "urllib"](https://docs.python.org/3/library/urllib.html)