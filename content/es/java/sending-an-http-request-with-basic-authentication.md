---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---

## ¿Qué & Por qué?

El envío de solicitudes HTTP con autenticación básica es un proceso por el cual un programa provee credenciales de acceso al realizar una solicitud HTTP. Los programadores lo hacen para asegurar la comunicación con servidores que requieren autenticación, asegurando que sólo aquellos con credenciales válidas puedan interactuar con el servidor.

## Cómo hacerlo

Aquí hay un código simple en Java que envía una solicitud GET con autenticación básica.

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class Main {
  public static void main(String[] args) throws Exception {
    URL url = new URL("http://miServidor.com/resource");
    String datosUsuario = "usuario:contraseña";
    String comandoBasicAuth = "Basic " + Base64.getEncoder().encodeToString(datosUsuario.getBytes());

    HttpURLConnection conn = (HttpURLConnection) url.openConnection();
    conn.setRequestMethod("GET");
    conn.setRequestProperty("Authorization", comandoBasicAuth);

    int resultado = conn.getResponseCode();
    System.out.println("Codigo de respuesta HTTP : " + resultado);
  }
}
```
Cuando se ejecuta, la muestra de salida podría ser:

```
Codigo de respuesta HTTP : 200
```

La respuesta HTTP 200 significa que la petición fue exitosa.

## Buceo profundo

Históricamente, la autenticación básica ha sido una forma común de autenticar solicitudes HTTP, aunque ha sido ampliamente reemplazada en muchos casos por métodos de autenticación más seguros. Sin embargo, su simplicidad hace que siga siéndola opción preferida en contextos donde la seguridad no es crítica.

Los programadores también tienen la opción de utilizar otras formas de autenticación, como OAuth o tokens JWT. Sin embargo, la autenticación básica sigue siendo útil por su simplicidad y amplia compatibilidad entre sistemas.

Implementar la autenticación básica con solicitudes HTTP en Java implica configurar una conexión HTTP, añadiendo un encabezado "Authorization" que contiene las credenciales codificadas en Base64.

## Ver También 

Para más detalles y prácticas recomendadas, puede consultar estas fuentes:

- Documentación [MDN en Autenticación HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)

Nota: La autenticación básica transmite contraseñas en texto plano (aunque en Base64) y no es segura si no se usa junto al protocolo HTTPS. Considere métodos de autenticación más seguros para aplicaciones en producción.