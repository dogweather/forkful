---
title:                "Envoyer une requête http"
html_title:           "Java: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi quelqu'un enverrait-il une requête HTTP? Cela peut sembler compliqué, mais c'est en fait une méthode simple et efficace pour communiquer avec des serveurs distants. En utilisant Java, vous pouvez facilement envoyer des requêtes HTTP et récupérer des données à partir de ressources en ligne telles que des API ou des sites Web.

## Comment faire

Pour envoyer une requête HTTP en utilisant Java, vous avez besoin de trois éléments principaux: une URL, une connexion et une manière de récupérer les données renvoyées par le serveur.

Tout d'abord, vous devez définir une URL valide en utilisant la classe URL de Java. Ensuite, vous devez établir une connexion en utilisant le protocole HTTP. Enfin, vous pouvez utiliser des objets InputStream et BufferedReader pour lire les données renvoyées par le serveur. Voici un exemple de code simple pour envoyer une requête HTTP et lire les données renvoyées:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.InputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public static void main(String[] args) throws IOException {
    // Définir l'URL à partir de laquelle nous voulons récupérer des données
    URL url = new URL("https://example.com/api/data");
    
    // Ouvrir une connexion HTTP vers l'URL
    HttpURLConnection conn = (HttpURLConnection) url.openConnection();
    
    // Vérifier que la connexion a réussi
    if (conn.getResponseCode() == HttpURLConnection.HTTP_OK) {
        // Lire les données renvoyées par le serveur
        InputStream inputStream = conn.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
        
        // Parcourir les données ligne par ligne
        String line;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }
    }
    
    // Fermer la connexion
    conn.disconnect();
}
```

Voici un exemple de sortie que nous pourrions obtenir en utilisant cet exemple de code:

```Java
{"name": "John", "age": 30, "city": "Paris"}
```

Cela démontre comment nous pouvons récupérer des données en utilisant une requête HTTP en Java.

## Plongée en profondeur

Il existe différents types de requêtes HTTP que vous pouvez envoyer en utilisant Java, tels que GET, POST, PUT et DELETE. Vous pouvez également spécifier des paramètres et des en-têtes personnalisés dans votre requête.

De plus, vous pouvez utiliser des bibliothèques tierces telles que Apache HTTP Components ou OkHttp pour faciliter l'envoi de requêtes HTTP et la gestion des réponses.

Pour en savoir plus sur la façon d'envoyer des requêtes HTTP en utilisant Java, consultez la documentation officielle de Java ou recherchez des tutoriels en ligne.

## Voir aussi

- [Documentation officielle de Java](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [Tutoriel sur les requêtes HTTP en Java](https://www.baeldung.com/java-http-request)
- [Bibliothèque Apache HTTP Components](https://hc.apache.org/httpcomponents-client-ga/index.html)
- [Bibliothèque OkHttp](https://square.github.io/okhttp/)