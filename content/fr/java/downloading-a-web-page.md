---
title:                "Téléchargement d'une page web"
html_title:           "Java: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes curieux de voir comment un site web est construit, ou si vous avez besoin d'extraire des données spécifiques d'une page web, alors vous pourriez être intéressé par l'idée de télécharger une page web en utilisant Java.

## Comment faire

Télécharger une page web en utilisant Java est un processus relativement simple. Tout d'abord, vous aurez besoin d'utiliser la classe URL pour définir l'URL de la page que vous souhaitez télécharger. Ensuite, vous pouvez utiliser la classe HttpURLConnection pour ouvrir une connexion vers l'URL et obtenir le contenu de la page. Enfin, vous pouvez enregistrer le contenu dans un fichier ou le traiter directement dans votre code.

Voici un exemple de code Java pour télécharger une page web et l'enregistrer dans un fichier:

```Java
import java.io.*;
import java.net.*;

public class PageDownloader {

    public static void main(String[] args) {

        // Définition de l'URL de la page à télécharger
        String urlString = "https://www.example.com/page";

        try {

            // Ouverture d'une connexion vers l'URL
            URL url = new URL(urlString);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();

            // Obtention du contenu de la page en utilisant un InputStreamReader
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuilder content = new StringBuilder();
            while ((inputLine = reader.readLine()) != null) {
                content.append(inputLine);
            }
            reader.close();

            // Enregistrement du contenu dans un fichier
            File file = new File("page.html");
            FileWriter writer = new FileWriter(file);
            writer.write(content.toString());
            writer.flush();
            writer.close();

            System.out.println("La page a été téléchargée avec succès.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Plongée en profondeur

Si vous souhaitez en savoir plus sur le processus de téléchargement d'une page web en utilisant Java, voici quelques points importants à retenir :

- La classe URL représente une URL et offre plusieurs méthodes utiles pour accéder à des informations telles que le protocole, l'hôte et le chemin.
- La classe HttpURLConnection est une sous-classe de la classe URLConnection qui gère les connexions HTTP spécifiques. Elle offre des méthodes pour ouvrir une connexion, envoyer des requêtes et recevoir des réponses.
- Lors de l'ouverture d'une connexion vers une URL, si la page est protégée par un mot de passe, vous devrez utiliser les classes URLConnection et Authenticator pour fournir les informations d'authentification nécessaires.
- Le contenu d'une page web est renvoyé sous forme de flux de données, vous devez donc utiliser un InputStreamReader pour lire le contenu de la page.
- Pour traiter le contenu de la page directement dans votre code, vous pouvez utiliser un parseur HTML comme Jsoup.

## Voir aussi
- [La documentation officielle de Java pour la classe URL](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/URL.html)
- [La documentation officielle de Java pour la classe HttpURLConnection](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/HttpURLConnection.html)
- [Un tutoriel sur l'utilisation de Jsoup pour traiter le contenu HTML](https://www.baeldung.com/java-with-jsoup)
- [Un article sur l'authentification avec URLConnection](https://www.codejava.net/java-se/networking/java-httpurlconnection-follow-redirect-example)