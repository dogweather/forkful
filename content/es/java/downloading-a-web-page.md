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

## Por qué

Descargar una página web usando Java es una habilidad valiosa para cualquier programador. Te permite acceder a contenido en línea y manipularlo según tus necesidades. Además, aprender cómo hacerlo puede mejorar tus habilidades en codificación y abrirte a nuevas posibilidades de proyectos.

## Cómo hacerlo

Para descargar una página web usando Java, primero debes importar la librería "java.net.*". Luego, sigue los siguientes pasos:

1. Crea un objeto de URL usando la dirección de la página web que quieras descargar.
2. Usa el método openConnection() para establecer una conexión con la página.
3. Usa el método getInputStream() para obtener un flujo de entrada de la página.
4. Usa un bucle para leer los datos del flujo de entrada y guardarlos en un String o escribirlos en un archivo.
5. Cierra la conexión una vez que hayas terminado.

A continuación, se muestra un ejemplo de código que descarga la página de inicio de Google y la imprime en la consola:

```Java
import java.net.*;

public class DownloadPage {

  public static void main(String[] args) {
    try {
      URL url = new URL("https://www.google.com/");
      URLConnection connection = url.openConnection();
      BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String inputLine;
      while ((inputLine = reader.readLine()) != null) {
        System.out.println(inputLine);
      }
      reader.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```

La salida de este código será el código HTML de la página de inicio de Google. Puedes manipular este string según tus necesidades, como guardarla en un archivo o buscar ciertas etiquetas para extraer información específica.

## Profundizando

Si quieres profundizar en el tema, aquí tienes algunas cosas que puedes considerar:

1. Utilizar el método setRequestProperty() para añadir encabezados a la solicitud de conexión, por ejemplo, para establecer el agente de usuario.
2. Usar una librería como Jsoup para analizar y manipular el HTML de la página descargada.
3. Aprender sobre los diferentes tipos de solicitudes que se pueden hacer, como GET y POST, y cómo incluir parámetros.
4. Aprender sobre manejo de errores y excepciones en caso de que la página no se pueda descargar correctamente.

¡Con estas herramientas y conocimientos, puedes descargar y manipular páginas web en Java como un profesional!

## Ver también

- Documentación oficial de la clase URL en Java: https://docs.oracle.com/javase/8/docs/api/java/net/URL.html
- Tutorial sobre descarga de páginas web en Java: https://www.geeksforgeeks.org/downloading-web-page-using-java/