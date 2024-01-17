---
title:                "Téléchargement d'une page web."
html_title:           "Java: Téléchargement d'une page web."
simple_title:         "Téléchargement d'une page web."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Télécharger une page web, c'est simplement récupérer le contenu d'un site internet. Les programmeurs font cela pour accéder à des données ou pour automatiser des tâches telles que le scraping de données.

## Comment faire:

Voici un exemple de code en Java pour télécharger une page web:

```
public static void main(String[] args) {
    String url = "https://www.example.com/";
    try {
        URL website = new URL(url);
        BufferedReader in = new BufferedReader(new InputStreamReader(website.openStream()));
        String inputLine;
        while ((inputLine = in.readLine()) != null)
            System.out.println(inputLine);
        in.close();
    } catch (IOException e) {
        e.printStackTrace();
    }
}
```

Et voici ce que vous obtiendrez comme résultat:

```
<!DOCTYPE html>
<html>
<head>
    <title>Exemple</title>
</head>
<body>
    <h1>Bienvenue sur cet exemple</h1>
    <p>Ceci est un site de démonstration.</p>
</body>
</html>
```

## Plongée en profondeur:

Télécharger une page web est un processus essentiel pour les programmeurs qui travaillent avec des données dynamiques ou qui développent des bots ou des scrapers. Avant l'avènement du web, le téléchargement de fichiers était beaucoup plus complexe et impliquait souvent l'utilisation de protocoles de transfert de fichiers tels que FTP. Aujourd'hui, avec les outils disponibles en Java, télécharger une page web est simple et efficace.

D'autres alternatives pour le téléchargement de pages web incluent l'utilisation d'outils tels que wget ou cURL en ligne de commande. Cependant, en utilisant Java, vous avez le contrôle total sur le processus et vous pouvez facilement intégrer le téléchargement dans vos projets.

Pour télécharger une page web dans Java, on utilise la classe URL pour définir l'URL du site à télécharger. Ensuite, on utilise les classes BufferedReader et InputStreamReader pour lire le contenu de la page et l'afficher en tant que chaîne de caractères.

## À lire également:

Consultez la documentation officielle de Java pour en savoir plus sur la classe URL et les outils disponibles pour le téléchargement de pages web en Java. Vous pouvez également consulter des tutoriels en ligne ou des forums pour voir comment d'autres programmeurs utilisent le téléchargement de pages web dans leurs projets.