---
title:                "Envoyer une demande http avec authentification de base"
html_title:           "Java: Envoyer une demande http avec authentification de base"
simple_title:         "Envoyer une demande http avec authentification de base"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Une requête HTTP avec une authentification de base est une méthode pour sécuriser une communication entre un client et un serveur en utilisant des identifiants de connexion. Les programmeurs utilisent cette méthode pour empêcher l'accès non autorisé à leurs serveurs et pour garantir la confidentialité des données échangées.

## Comment ça marche:
Voici un exemple de code Java pour envoyer une requête HTTP avec une authentification de base:

```Java
URL url = new URL("https://exemple.com/api/endpoint");

String username = "utilisateur";
String password = "motdepasse";

HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");
//Ajoute les identifiants dans l'en-tête de la requête
String encodedCredentials = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
con.setRequestProperty("Authorization", "Basic " + encodedCredentials);

//Code pour lire et traiter la réponse de la requête
```

En utilisant la classe URL et HttpURLConnection de Java, nous pouvons établir une connexion avec un endpoint et spécifier un username et un mot de passe pour l'authentification. Ensuite, nous ajoutons ces informations dans l'en-tête de la requête en les encodant en base64.

La réponse de la requête peut être lue et traitée grâce à la méthode "getInputStream" de HttpURLConnection.

## Plongée en profondeur:
Cette méthode d'authentification a été introduite en 1999 dans la spécification HTTP/1.0 et est devenue une méthode très populaire pour sécuriser les communications sur le Web. Une alternative à cette méthode est l'utilisation du protocole HTTPS, qui chiffre toutes les données échangées entre le client et le serveur.

En ce qui concerne l'implémentation, les identifiants peuvent être stockés en clair dans le code source, mais cela peut être dangereux en cas de vol ou d'accès non autorisé. Une meilleure pratique consiste à stocker les identifiants dans un fichier de configuration externe ou dans une variable d'environnement.

## Voir aussi:
- [Java URL Class Documentation](https://docs.oracle.com/javase/10/docs/api/java/net/URL.html)
- [Java HttpURLConnection Class Documentation](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [HTTP Basic Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication#Basic_authentication_scheme)