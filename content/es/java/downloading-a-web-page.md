---
title:                "Descargando una página web"
html_title:           "Java: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Descargar una página web es obtener su contenido en formato HTML, es decir, el código que le da forma y estructura. Los programadores suelen hacer esto para extraer información específica de la página web o para automatizar tareas en su trabajo.

## Cómo hacerlo:
Para descargar una página web en Java, podemos utilizar la biblioteca URL y su clase openStream (). El siguiente código muestra cómo obtener el contenido HTML de una página web y guardarlo en un archivo:

```Java
import java.io.*;
import java.net.*;

public class DescargarPaginaWeb {
    public static void main(String[] args) {
        try {
            // Establecer la URL de la página web
            URL url = new URL("https://www.example.com");
            
            // Abrir una conexión a la página web y obtener su contenido
            URLConnection connection = url.openConnection();
            InputStream inputStream = connection.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
            
            // Crear un archivo para guardar el contenido HTML
            File archivo = new File("pagina.html");
            
            // Leer el contenido de la página web y escribirlo en el archivo
            FileWriter writer = new FileWriter(archivo);
            String linea;
            while ((linea = reader.readLine()) != null) {
                writer.write(linea + "\n");
            }
            writer.close();
            
            // Imprimir un mensaje de éxito
            System.out.println("Página web descargada con éxito!");
        } catch (IOException e) {
            System.out.println("Error al descargar la página web.");
        }
    }
}
```

## Profundizando:
Descargar páginas web es una técnica muy usada en web scraping, que es el proceso de extraer información de páginas web de manera automatizada. También se utiliza para automatizar tareas en el mundo del desarrollo web.

Existen otras bibliotecas en Java que facilitan la descarga de páginas web, como Apache Commons HttpClient y Jsoup. Estas bibliotecas ofrecen más funcionalidades y opciones de configuración.

Es importante tener en cuenta que al descargar páginas web debemos respetar los términos de uso y derechos de autor de sus propietarios.

## Ver también:
- [Java URL library documentation](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Web scraping with Java](https://www.scrapingbee.com/blog/web-scraping-java/)