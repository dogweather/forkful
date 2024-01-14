---
title:                "Java: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi envoyer une requête HTTP avec une authentification de base ?

L'envoi d'une requête HTTP avec une authentification de base est courante dans la programmation Java car elle permet de sécuriser les échanges de données entre un client et un serveur. Cela permet également de vérifier l'identité de l'utilisateur qui envoie la requête et de restreindre l'accès à certaines ressources.

## Comment le faire :

Pour envoyer une requête HTTP avec une authentification de base en Java, nous pouvons utiliser la classe `URLConnection`. Voici un exemple de code :

```java
// Importer les classes nécessaires
import java.net.URL;
import java.net.URLConnection;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Base64;

// Définir l'URL et les informations d'authentification
String url = "https://example.com/api/users";
String username = "john";
String password = "p@ssword";

// Créer une instance de l'URL et ouvrir une connexion
URL userURL = new URL(url);
URLConnection con = userURL.openConnection();

// Encoder les informations d'authentification en base64
String authString = username + ":" + password;
String authEncoded = Base64.getEncoder().encodeToString(authString.getBytes());

// Ajouter l'en-tête d'authentification à la requête
con.setRequestProperty("Authorization", "Basic " + authEncoded);

// Envoyer la requête et récupérer la réponse
BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();

while ((inputLine = in.readLine()) != null) {
  response.append(inputLine);
}
in.close();

// Afficher la réponse
System.out.println(response.toString());
```

Dans cet exemple, nous encodons les informations d'authentification en base64 et les ajoutons à l'en-tête de notre requête avant de l'envoyer. Une fois la requête envoyée, nous pouvons récupérer la réponse du serveur et l'afficher.

## Plongeon en profondeur :

L'authentification de base est une méthode simple mais pas très sécurisée, car les informations d'identification sont envoyées en clair dans la requête. Il est donc conseillé de l'utiliser uniquement dans un environnement de développement ou pour des transactions non sensibles.

Pour une sécurité renforcée, il est recommandé d'utiliser des méthodes d'authentification telles que OAuth ou SSL/TLS. Il est également important de noter que l'utilisation d'une authentification de base ne dispense pas de la mise en place de mesures de sécurité supplémentaires, telles que la vérification de l'adresse IP ou l'utilisation de jetons d'authentification uniques.

## Voir aussi :

- [Documentation officielle de la classe URLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html)
- [Guide d'utilisation de l'API HttpURLConnection en Java](https://www.baeldung.com/java-http-request)
- [Article sur la sécurité des échanges de données en Java](https://dzone.com/articles/secure-data-exchange-in-java-using-basic-authentic)