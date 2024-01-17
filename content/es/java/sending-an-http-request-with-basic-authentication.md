---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Java: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es un proceso en el cual se envía una solicitud a un servidor web con credenciales de usuario incluidas. Los programadores lo hacen para acceder a información protegida o realizar acciones en sitios web y aplicaciones.

## Cómo hacerlo:
```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Base64;

public class BasicAuthHTTP {

  public static void main(String[] args) {

    try {
      // Crear URL de solicitud y conexión HTTP
      URL url = new URL("https://www.ejemplo.com/api/getInfo");
      HttpURLConnection con = (HttpURLConnection) url.openConnection();

      // Agregar credenciales de usuario en formato básico
      String username = "usuario";
      String password = "contraseña";
      String auth = username + ":" + password;
      byte[] encodedAuth = Base64.getEncoder().encode(auth.getBytes());
      String authHeaderValue = "Basic " + new String(encodedAuth);
      con.setRequestProperty("Authorization", authHeaderValue);

      // Establecer método de solicitud y enviar la solicitud
      con.setRequestMethod("GET");
      int responseCode = con.getResponseCode();

      // Leer la respuesta del servidor
      BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
      String inputLine;
      StringBuffer response = new StringBuffer();
      while ((inputLine = in.readLine()) != null) {
        response.append(inputLine);
      }
      in.close();

      // Imprimir respuesta
      System.out.println(response.toString());
      
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

## Inmersión profunda:
La autenticación básica se introdujo en HTTP 1.0 como un método de seguridad simple y ampliamente adoptado. Sin embargo, con la evolución de la tecnología, se ha vuelto más vulnerable a ataques de phishing y robo de identidad. Los programadores pueden optar por métodos de autenticación más seguros, como OAuth, para proteger mejor la información del usuario al enviar solicitudes HTTP.

## Véase también:
Ejemplos de uso de HTTP básica en aplicaciones Java: https://www.baeldung.com/java-http-request

Documentación oficial de Java sobre la clase HttpURLConnection: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html