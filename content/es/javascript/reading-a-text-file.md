---
title:    "Javascript: Leyendo un archivo de texto"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Porqué

Leer y entender archivos de texto es una habilidad esencial para cualquier programador de JavaScript. Esto permite acceder y manipular grandes cantidades de información en formato de texto, lo que a su vez puede ser utilizado para una amplia variedad de proyectos y aplicaciones. En este artículo, exploraremos cómo leer un archivo de texto utilizando JavaScript y por qué es una habilidad valiosa para cualquier desarrollador.

## Como Hacerlo

Para leer un archivo de texto en JavaScript, se requieren algunos pasos sencillos. Primero, necesitarás tener un documento HTML donde puedas escribir tu código JavaScript. Luego, puedes utilizar la función `XMLHttpRequest` para cargar el archivo de texto. Una vez que se ha cargado el archivo, se puede acceder a su contenido utilizando la propiedad `responseText` y guardarlo en una variable. A continuación, se puede utilizar el método `split` para separar el contenido del archivo en un array, dividido por líneas. Finalmente, puedes recorrer el array e imprimir cada línea en la consola, o realizar cualquier otra operación que desees con el contenido del archivo.

Un ejemplo de código sería el siguiente:

```Javascript
let xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        let fileContent = this.responseText;
        let lines = fileContent.split('\n');
        for (let i = 0; i < lines.length; i++) {
            console.log(lines[i]);
        }
    }
};
xhttp.open("GET", "archivo.txt", true);
xhttp.send();
```

Si se tiene un archivo de texto con el siguiente contenido:

```
¡Hola Mundo!
Soy un archivo de texto
en formato de líneas
```

La salida en la consola sería:

```
¡Hola Mundo!
Soy un archivo de texto
en formato de líneas
```


## Profundizando

Existen varias formas de leer un archivo de texto en JavaScript, además del método mostrado anteriormente. Una alternativa es utilizar la función `fetch`, que es una forma moderna de realizar solicitudes de recursos HTTP. Esta función utiliza promesas para manejar la respuesta del servidor y simplifica el proceso de lectura de un archivo de texto.

Otra opción es utilizar librerías o frameworks externos como `Node.js` o `jQuery` que tienen funciones específicas para trabajar con archivos de texto.

También es importante mencionar que no solo se puede leer el contenido de un archivo de texto, sino que también se pueden realizar otras operaciones como escribir en el archivo o buscar información específica en él.

En resumen, leer y comprender archivos de texto es una habilidad importante para cualquier programador de JavaScript. Con los pasos mencionados anteriormente y con práctica, podrás dominar esta habilidad y utilizarla en tus proyectos de forma efectiva.

## Ver También

- [XMLHttpRequest en w3schools](https://www.w3schools.com/xml/xml_http.asp)
- [Función fetch en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/API/Fetch_API/Utilizando_Fetch)
- [Documentación de Node.js](https://nodejs.org/es/docs/)
- [Documentación de jQuery](https://api.jquery.com/)