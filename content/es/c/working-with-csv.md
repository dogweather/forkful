---
title:                "Trabajando con csv"
html_title:           "C: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-csv.md"
---

{{< edit_this_page >}}

¡Hola programadores! ¿Alguna vez has oído hablar de CSV? Es uno de los formatos de archivo más utilizados en la programación y hoy te contaré por qué y cómo trabajar con él en C.

## ¿Qué y por qué?

CSV significa "valores separados por comas" y se utiliza como un formato de archivo para almacenar datos en forma de tabla, donde cada valor se separa por una coma. Los programadores trabajan con archivos CSV porque es fácil de leer y escribir con herramientas de programación, además de ser compatible con muchos otros programas.

## Cómo hacerlo:

Para trabajar con archivos CSV en C, necesitarás la biblioteca estándar "stdlib.h". Primero, debes abrir el archivo CSV con la función "fopen" y especificar el modo de lectura o escritura. Luego, puedes usar la función "fscanf" para leer los datos en formato CSV y guardarlos en variables. Aquí hay un ejemplo de cómo leer una tabla de nombres y edades:

```
FILE *archivo_csv = fopen("nombre_archivo.csv", "r"); // modo de lectura
char nombre[20];
int edad;

while (fscanf(archivo_csv, "%s, %d", nombre, &edad) != EOF) { // mientras no sea el final del archivo
    printf("Nombre: %s, Edad: %d\n", nombre, edad); // imprimir los datos leídos
}

fclose(archivo_csv); // cerrar el archivo al terminar
```

## Profundizando:

Los archivos CSV se originaron en los años 70 como una forma de compartir datos entre programas de hojas de cálculo. Aunque es un formato popular, también hay otras opciones, como JSON o XML, que son más legibles para los humanos. Sin embargo, CSV sigue siendo ampliamente utilizado debido a su simplicidad y compatibilidad con diferentes programas. En cuanto a la implementación, es importante tener en cuenta que los datos entre comas pueden ser fácilmente afectados por las comillas, espacios o saltos de línea, por lo que es necesario manejar estos casos en el código para evitar errores.

## Véase también:

Si quieres conocer más sobre cómo trabajar con archivos CSV en C, te recomiendo que le eches un vistazo a la documentación de las funciones "fopen" y "fscanf". También puedes explorar otros lenguajes de programación que te permiten manipular fácilmente archivos CSV, como Python o Java.

¡Eso es todo por ahora! Espero que este artículo te haya ayudado a comprender mejor qué es y cómo utilizar archivos CSV en C. ¡Hasta la próxima!