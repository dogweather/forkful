---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# L'envoi de requêtes HTTP avec une authentification de base en Java

## Quoi et Pourquoi?
L'envoi d'une requête HTTP avec une authentification de base est une manière de sécuriser l'accès à des ressources sur le web. Les programmeurs l'utilisent pour s'assurer que seuls les utilisateurs autorisés peuvent accéder à des données précises.

## Comment faire:
Pour envoyer une requête HTTP avec une authentification de base en Java, vous avez besoin d'une URL, d'un nom d'utilisateur et d'un mot de passe. Voici un exemple de code:

```Java
import java.net.*;
import java.io.*;

public class HttpBasicAuth {

    public static void main(String[] args) throws Exception {

        String urlStr = "http://myurl";
        String user = "username";
        String password = "password";

        URL url = new URL(urlStr);
        URLConnection urlConnection = url.openConnection();
        String authStr = user + ":" + password;
        String authEncoded = Base64.getEncoder().encodeToString(authStr.getBytes());

        urlConnection.setRequestProperty("Authorization", "Basic " + authEncoded);

        BufferedReader in = new BufferedReader(new InputStreamReader(urlConnection.getInputStream()));

        String inputLine;
        while ((inputLine = in.readLine()) != null) {
            System.out.println(inputLine);
        }
        in.close();
    }
}
```
Cet exemple vous donnera la sortie attendue de la ressource que vous essayez d'accéder.

## Plongée en profondeur
Historiquement, l'authentification de base a été largement utilisée par les protocoles HTTP et SMTP. Cependant, elle n'est pas très sécurisée car les informations d'identification sont transmises en clair (bien que codées en Base64, ce qui est facilement décodable).
Il existe également des alternatives à l'authentification de base, notamment l'authentification Digest, l'authentification par formulaire, l'authentification par jeton et l'authentification OAuth.

Dans le code ci-dessus, nous utilisons la méthode `setRequestProperty()` pour ajouter un en-tête HTTP `Authorization` contenant nos informations d'authentification codées. Cela envoie essentiellement nos informations d'identification à l'URL que nous essayons d'atteindre avec chaque requête.

## Voir Aussi
1. [Authentification HTTP sur Wikipedia](https://fr.wikipedia.org/wiki/Authentification_HTTP)
2. [API Java pour URLConnection](https://docs.oracle.com/javase/7/docs/api/java/net/URLConnection.html)
3. [Base64 en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)