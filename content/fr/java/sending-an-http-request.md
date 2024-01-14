---
title:                "Java: Envoi d'une demande http"
simple_title:         "Envoi d'une demande http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi
Les requêtes HTTP sont essentielles pour communiquer avec les serveurs et récupérer des données dans les applications Java. En comprenant comment les envoyer efficacement, vous pouvez améliorer les performances de votre application et offrir une meilleure expérience utilisateur.

## Comment faire
### Importer les packages nécessaires
Pour envoyer une requête HTTP en Java, nous avons besoin d'importer deux packages : java.net et java.io. Le premier contient la classe URL qui nous permet de spécifier l'adresse du serveur et la seconde contient la classe InputStream qui nous permet de lire la réponse du serveur.

```Java
import java.net.URI;
import java.net.URISyntaxException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
```

### Créer une URL
Maintenant, nous pouvons créer une instance de la classe URL en utilisant le constructeur qui prend une chaîne d'URL en paramètre. Nous pouvons également utiliser la classe URI pour gérer les caractères spéciaux dans l'URL.

```Java
String urlString = "https://www.monsite.com/donnees";
URI uri = new URI(urlString);
URL url = uri.toURL();
```

### Ouvrir une connexion
Pour envoyer une requête, nous devons ouvrir une connexion avec le serveur en utilisant la méthode openConnection() de l'instance URL. Ensuite, nous pouvons définir le type de requête en utilisant la méthode setRequestMethod() et ajouter des en-têtes si nécessaire.

```Java
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");
connection.setRequestProperty("Accept", "application/json");
```

### Lire la réponse
Enfin, nous pouvons lire la réponse du serveur en utilisant un InputStream et le convertir en une chaîne ou un objet Java en fonction du format de la réponse.

```Java
int responseCode = connection.getResponseCode();
// Lecture de la réponse du serveur
if (responseCode == HttpURLConnection.HTTP_OK) {
    InputStream inputStream = connection.getInputStream();
    BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
    StringBuffer response = new StringBuffer();
    String inputLine;
    while ((inputLine = bufferedReader.readLine()) != null) {
        response.append(inputLine);
    }
    bufferedReader.close();
    // Utilisation de la réponse
    System.out.println(response.toString());
}
```

## Plongée profonde
Il existe plusieurs méthodes pour envoyer une requête HTTP en Java, telles que l'utilisation de la classe HttpClient de Apache ou de la librairie OkHttp. Il est également important de gérer les erreurs et les exceptions lors de l'envoi de requêtes et de gérer les autorisations si nécessaire.

# Voir aussi
- [Java URL class documentation](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Handling URL encoded characters in Java](https://www.baeldung.com/java-url-encoding-decoding)