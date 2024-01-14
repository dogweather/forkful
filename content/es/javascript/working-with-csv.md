---
title:                "Javascript: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

# Porqué trabajar con CSV en Javascript

CSV, o valores separados por comas, es un formato de archivo comúnmente utilizado para almacenar datos en una tabla. Muchas veces, los archivos CSV se utilizan para importar o exportar datos entre diferentes programas. En el contexto de la programación en Javascript, trabajar con CSV puede ser beneficioso para manipular grandes cantidades de datos de forma eficiente y precisa. En este artículo, exploraremos por qué y cómo trabajar con CSV en Javascript.

## Cómo hacerlo

Para empezar, es importante mencionar que Javascript tiene una función integrada llamada `fetch` que se puede utilizar para leer archivos CSV. Esta función hace una llamada a una URL, recuperando la información almacenada en un objeto `Response`. A continuación, se puede utilizar el método `text()` para obtener los datos de ese objeto y luego convertirlos a un formato legible.

Por ejemplo, si tenemos un archivo CSV llamado "datos.csv" que contiene información sobre nombres y edades, podríamos leerlo de la siguiente manera:

```Javascript
fetch('datos.csv')
  .then(response => response.text())
  .then(data => console.log(data));
```

Esto imprimirá en la consola el contenido del archivo CSV en formato de texto. A partir de ahí, se pueden aplicar diferentes procesos y manipulaciones de datos según sea necesario.

## Profundizando

Ahora que sabemos cómo leer un archivo CSV en Javascript, es importante comprender cómo manipular los datos dentro del CSV. Una forma podría ser convertir los datos a un formato de objeto utilizando el método `split()` y luego encontrar y filtrar ciertos valores según sea necesario.

Por ejemplo, podemos tener un archivo CSV con información de diferentes ciudades, como el nombre, la población y el país.

```Javascript
Ciudad, Población, País
Londres, 8908081, Reino Unido
Barcelona, 5511960, España
New York, 8398748, Estados Unidos
```

Para convertir esto en un objeto legible en Javascript, podemos hacer lo siguiente:

```Javascript
fetch('datos.csv')
  .then(response => response.text())
  .then(data => {
    // Convertir a un array separando por líneas
    const filas = data.split('\n');
    // Eliminar la primera fila de encabezado
    const encabezado = filas.shift().split(',');
    // Crear objeto vacío para almacenar los datos
    const ciudades = {};
    
    // Recorrer cada fila y añadir la información al objeto
    filas.forEach(row => {
      const columna = row.split(',');
      // Puedes hacer operaciones aquí para filtrar ciudades de cierto país o con cierta población
      ciudades[columna[0]] = {
        poblacion: columna[1],
        pais: columna[2]
      };
    });
    
    // Imprimir la información de las ciudades
    console.log(ciudades);
  });
```

## Ver también

- [Documentación oficial de fetch en MDN](https://developer.mozilla.org/es/docs/Web/API/Fetch_API/Utilizando_Fetch)
- [Método split() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/split)
- [Manipulación de archivos CSV en Javascript – Blog de Medium](https://medium.com/javascript-in-plain-english/how-to-work-with-csv-files-using-javascript-2199f963a3c8)