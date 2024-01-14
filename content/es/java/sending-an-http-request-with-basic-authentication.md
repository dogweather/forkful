---
title:                "Java: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué
Enviar una solicitud HTTP con autenticación básica es esencial para la seguridad de una aplicación. Al utilizar la autenticación básica, se requiere un usuario y una contraseña para acceder a una determinada solicitud, lo que ayuda a proteger la información sensible y evitar accesos no autorizados.

## Cómo hacerlo
Para enviar una solicitud HTTP con autenticación básica en Java, necesitaremos utilizar la clase `HttpURLConnection` y establecer las credenciales mediante el encabezado `Authorization` en el formato `Basic <username>:<password>`. Aquí hay un ejemplo de código utilizando esta clase:

```
Java
// Importar las clases de Java necesarias
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;

// URL de la solicitud
URL url = new URL("https://miaplicacion.com/endpoint");

// Abrir la conexión utilizando la URL
HttpURLConnection con = (HttpURLConnection) url.openConnection();
 
// Establecer el método de solicitud como GET
con.setRequestMethod("GET");
 
// Establecer las credenciales utilizando el encabezado Authorization
String username = "usuario";
String password = "contraseña";
String encodedCredentials = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
con.setRequestProperty("Authorization", "Basic " + encodedCredentials);
 
// Obtener la respuesta y leerla como una cadena
 BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String output;
StringBuilder response = new StringBuilder();
while ((output = in.readLine()) != null) {
	response.append(output);
}
in.close();
 
// Imprimir la respuesta
System.out.println("Respuesta: " + response.toString());
```

El resultado de este ejemplo sería algo similar a:

```
Respuesta: { "message": "¡Hola, usuario!" }
```

## Profundizando
Hay algunas cosas a tener en cuenta al enviar una solicitud HTTP con autenticación básica. Primero, es importante asegurarse de que la URL sea válida y esté bien formada. Además, es vital verificar la documentación de la API o la plataforma con la que se está comunicando para asegurarse de utilizar el formato correcto para las credenciales en el encabezado de autorización. También es recomendable utilizar métodos de encriptación para asegurar aún más las contraseñas antes de enviarlas a través de una solicitud HTTP.

## Ver también
- [Documentación de la clase `HttpURLConnection` en Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/HttpURLConnection.html)
- [Ejemplo de autenticación básica en PHP](https://www.php.net/manual/es/features.http-auth.php)
- [Ejemplo de autenticación básica en Python utilizando la librería `requests`](https://realpython.com/python-requests/)