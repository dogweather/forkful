---
title:                "Descargando una página web"
html_title:           "TypeScript: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web puede ser útil en diferentes situaciones, como por ejemplo para obtener información específica, guardar contenido para accederlo sin conexión o simplemente por curiosidad. En este artículo, aprenderemos cómo descargar una página web utilizando TypeScript.

## Cómo hacerlo

```TypeScript 
const request = require('request');
const fs = require('fs');

request('www.example.com', (error, response, body) => {
    if (!error && response.statusCode === 200) {
        fs.writeFile('example.html', body, (error) => {
            if (error) {
                console.log('Hubo un error al guardar el archivo');
            } else {
                console.log('La página web se ha descargado con éxito');
            }
        });
    } else {
        console.log('Hubo un error al hacer la solicitud');
    }
});
```

En este ejemplo, estamos utilizando el módulo `request` para hacer una solicitud a la página web en la URL especificada. Luego, utilizamos el módulo `fs` para guardar el contenido de la respuesta en un archivo HTML. Si la solicitud es exitosa, el archivo se guardará en la carpeta actual con el nombre `example.html`. Si hay algún error, se mostrará un mensaje de error en la consola.

## Profundizando en el tema

Al descargar una página web, es importante tener en cuenta que cualquier contenido dinámico (como elementos cargados con JavaScript) no se descargará. Solo se obtendrá el código HTML que está presente en el momento de la descarga. Además, es posible que algunos sitios web requieran autenticación o bloqueen solicitudes malintencionadas, por lo que es posible que no se pueda descargar la página deseada. Otro detalle a tener en cuenta es que al descargar una página web, se está consumiendo el ancho de banda del servidor al hacer una solicitud, por lo que se debe utilizar esta técnica con responsabilidad.

## Ver también

- [Cómo hacer solicitudes HTTP en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-the-request-module-in-node-js)
- [Documentación oficial de TypeScript](https://www.typescriptlang.org/docs/)
- [Node.js - documentación oficial](https://nodejs.org/es/docs/)