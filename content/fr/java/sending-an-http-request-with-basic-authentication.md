---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Java: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait utile d'envoyer une demande HTTP avec une authentification de base en Java. Eh bien, dans un monde où la sécurité est de plus en plus importante, l'authentification de base offre une méthode simple et efficace pour protéger votre application contre les accès non autorisés.

## Comment faire

Tout d'abord, vous devez importer les packages nécessaires de Java pour envoyer une demande HTTP. Ensuite, vous devez créer un objet `URLConnection` en spécifiant l'URL de la demande et en l'ouvrant avec la méthode `openConnection()`. Enfin, vous devez définir les informations d'authentification en utilisant la méthode `setRequestProperty()` et envoyer la demande en utilisant la méthode `getInputStream()`.

Voici un exemple de code pour envoyer une demande HTTP avec une authentification de base en Java :

```Java
import java.io.*;
import java.net.*;
public class AuthentificationDeBase {

    public static void main(String[] args) throws IOException {

        // Spécifiez l'URL de la demande
        URL url = new URL("http://www.example.com");

        // Ouvrez une connexion avec l'URL
        URLConnection conn = url.openConnection();

        // Définissez l'authentification de base
        String authentication = "Basic " + javax.xml.bind.DatatypeConverter.printBase64Binary("utilisateur:motdepasse".getBytes());
        conn.setRequestProperty("Authorization", authentication);

        // Envoyez la demande et obtenez la réponse
        InputStream in = conn.getInputStream();

        // Affichez la réponse
        int c;
        while ((c = in.read()) != -1) {
            System.out.write(c);
        }
    }
}
```

L'exemple ci-dessus montre comment utiliser l'authentification de base pour envoyer une demande HTTP en Java. Notez que les identifiants de connexion doivent être encodés en base64 avant d'être envoyés.

## Plongée en profondeur

Maintenant que vous savez comment envoyer une demande HTTP avec une authentification de base en Java, parlons un peu plus en détail de cette méthode. L'authentification de base fonctionne en ajoutant un en-tête d'authentification à votre demande avec les identifiants de connexion encodés en base64. Cela signifie que vos identifiants seront visibles dans le texte de la demande, il est donc recommandé d'utiliser HTTPS pour une sécurité renforcée.

Il est également important de noter que l'authentification de base n'est pas une méthode très sécurisée car les identifiants peuvent être facilement décryptés en utilisant des outils en ligne. Pour une sécurité renforcée, il est préférable d'utiliser d'autres méthodes comme l'authentification à clé publique ou l'authentification à jeton.

## Voir aussi

Maintenant que vous connaissez les bases de l'envoi d'une demande HTTP avec une authentification de base en Java, vous pouvez explorer d'autres façons de renforcer la sécurité de vos applications en utilisant des méthodes d'authentification plus avancées :

- [Authentification à clé publique en Java](https://www.baeldung.com/java-public-key-authentication)
- [Authentification à jeton en Java](https://developer.okta.com/blog/2018/09/13/build-a-secure-token-service-in-java-with-jwt)

Vous pouvez également consulter la documentation officielle de Java pour en savoir plus sur les classes et les méthodes utilisées pour envoyer une demande HTTP.