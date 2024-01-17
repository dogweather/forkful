---
title:                "Envoi d'une demande http"
html_title:           "Java: Envoi d'une demande http"
simple_title:         "Envoi d'une demande http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'envoi d'une requête HTTP est le moyen pour un programme informatique de demander des informations à un serveur web. Les programmeurs le font pour obtenir des données en temps réel ou pour intégrer des fonctionnalités interactives dans leurs applications.

## Comment le faire:

Dans Java, vous pouvez utiliser la classe HttpURLConnection pour envoyer une requête HTTP. Voici un exemple de code:

```
URL url = new URL("https://www.example.com/api");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");

int responseCode = con.getResponseCode();
System.out.println("Code de réponse: " + responseCode);

BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));

String inputLine;
StringBuffer response = new StringBuffer();
while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

System.out.println("Réponse: " + response.toString());
```

Cela enverra une requête GET à l'URL spécifiée et affichera le code de réponse et la réponse du serveur.

## Plongée en profondeur:

L'envoi d'une requête HTTP est un aspect essentiel de la communication entre le client et le serveur dans le développement web. Historiquement, les programmeurs utilisaient la classe HttpURLConnection, mais depuis Java 11, il est recommandé d'utiliser la classe HttpClient pour une meilleure gestion des connexions et des requêtes.

Il existe également des alternatives telles que l'utilisation de bibliothèques externes comme Apache HttpComponents ou OkHttp.

Pour comprendre le processus d'envoi d'une requête HTTP, il est important de connaître les différents types de méthodes de requête (GET, POST, PUT, etc.) et leur utilisation appropriée, ainsi que la structure d'une requête et d'une réponse HTTP.

## Voir aussi:

- Guide Java sur l'utilisation de HttpURLConnection: https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html
- Documentation officielle de la classe HttpClient: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html
- Utilisation d'Apache HttpComponents: https://www.baeldung.com/httpclient-guide
- Utilisation d'OkHttp: https://square.github.io/okhttp/