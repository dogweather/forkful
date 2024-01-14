---
title:                "Gleam: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##¿Por qué deberías descargar una página web? 

Descargar una página web puede ser útil para una variedad de propósitos, desde realizar análisis de datos hasta realizar pruebas en el desarrollo de una aplicación web. También puede ser una excelente manera de acceder a contenido sin conexión.

## Cómo hacerlo: 

Para descargar una página web en Gleam, puedes utilizar la función `httpc.get` y especificar la URL de la página que deseas descargar. Por ejemplo: 

```Gleam 
program = httpc.get("https://www.mi-pagina.com") 
``` 

Una vez que tengas la página descargada, puedes utilizar el módulo `html_parser` para analizar su contenido y extraer la información que necesites. Por ejemplo:

```Gleam
parsed_html = html_parser.parse(program.body)
title = parsed_html."div > h1".inner_text
```

En este ejemplo, estamos utilizando el selector de etiqueta CSS para extraer el texto dentro de la etiqueta `h1` dentro de un `div` en la página.

## Profundizando: 

La función `httpc.get` también permite especificar cabeceras HTTP adicionales y realizar acciones como seguir redireccionamientos o establecer límites de tiempo de espera.

Además, el módulo `html_parser` ofrece una variedad de métodos para manipular y extraer información de manera más precisa y específica de una página web.

## Ver también: 

- Documentación de `httpc.get`: https://gleam.run/modules/httpc.html#get-2 
- Documentación de `html_parser`: https://gleam.run/modules/html_parser.html