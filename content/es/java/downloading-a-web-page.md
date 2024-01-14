---
title:                "Java: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué descargar una página web?

Descargar una página web puede ser útil para varios propósitos, como guardar una copia de seguridad de un sitio importante, acceder a contenido offline o analizar la estructura y el código de una página en particular.

## Cómo hacerlo

Para descargar una página web en Java, podemos utilizar la clase URL y la clase URLConnection. Primero, debemos crear un objeto URL que represente la dirección de la página que queremos descargar:

```Java
URL url = new URL("https://www.ejemplo.com");
```

Luego, podemos utilizar el método openConnection() para establecer una conexión con la página:

```Java
URLConnection connection = url.openConnection();
```

Una vez que tenemos la conexión establecida, podemos utilizar la clase BufferedReader para leer los datos de la página:

```Java
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
```

Y finalmente, podemos guardar los datos en un archivo utilizando la clase FileWriter:

```Java
FileWriter fileWriter = new FileWriter("ejemplo.html");
String input;
while ((input = reader.readLine()) != null) {
  fileWriter.write(input);
}
fileWriter.close();
```

Con estos pasos, hemos descargado la página web y guardado su contenido en un archivo llamado "ejemplo.html".

## Profundizando

Para descargar una página web de manera eficiente, también podemos utilizar la biblioteca Open Source Jsoup. Esta biblioteca nos permite parsear y manipular HTML de manera fácil y rápida.

Para utilizar Jsoup, primero debemos agregar su dependencia en nuestro proyecto:

```Java
dependencies {
    implementation 'org.jsoup:jsoup:1.13.1'
}
```

Luego, podemos utilizar su método connect() para obtener una conexión a la página y descargar su contenido:

```Java
Document document = Jsoup.connect("https://www.ejemplo.com").get();
```

También podemos usar select() para seleccionar elementos específicos de la página y obtener su contenido:

```Java
Elements elements = document.select("div.precio");
String precio = elements.text();
```

Con Jsoup, podemos realizar tareas más complejas, como loguearnos en una página y descargar información exclusiva para usuarios registrados.

## Ver también

Aquí hay algunos recursos adicionales en español que pueden ser útiles para descargar una página web en Java:

- [Tutorial básico de descarga de páginas web en Java](https://jarroba.com/descargar-una-pagina-web-en-java/)
- [Descargar una página web utilizando las clases URL y URLConnection](https://codigofacilito.com/articulos/descargar-na-pagina-web-java)
- [Docs de la biblioteca Jsoup en español](https://jsoup.org/cookbook/extracting-data/working-with-urls)