---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Télécharger une page web signifie récupérer et stocker son contenu pour une utilisation ultérieure. Les programmeurs le font souvent pour analyser le contenu, récupérer des données ou tester le site web.

## Comment faire :

Nous utiliserons `java.net.URL` et `java.nio.file` pour simplifier le téléchargement. Voici un exemple de base.

```Java
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

public class Downloader {
    public static void main(String[] args) throws Exception {
        URL website = new URL("http://example.com");
        try (InputStream in = website.openStream()) {
            Files.copy(in, Path.of("output.html"), StandardCopyOption.REPLACE_EXISTING);
        }
    }
}
```

Cette opération téléchargera la page d'exemple et la sauvegardera dans un fichier "output.html". 

## Plongée en profondeur :

Java a une riche histoire de gestion des URL et du réseau. `java.net.URL` date de Java 1.0, mais pour une analyse plus poussée, vous pourriez vouloir examiner HttpClient, introduit dans Java 11. 

En termes d'alternatives, vous pourriez regarder Jsoup qui facilite encore plus le traitement des documents HTML, ou des bibliothèques comme OkHttp pour des options plus robustes.

Notez que le code ci-dessus est simple, mais n'implémente pas de contrôle d'erreur, vous voudriez certainement ajouter une gestion des exceptions en cas de problème réseau. 

## Voir aussi :

- Java Networking Tutorial : https://docs.oracle.com/javase/tutorial/networking/urls/index.html
- HttpClient Documentation : https://openjdk.java.net/groups/net/httpclient/intro.html
- Jsoup Documentation : https://jsoup.org/
- OkHttp Documentation : https://square.github.io/okhttp/