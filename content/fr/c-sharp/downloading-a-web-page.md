---
title:                "C#: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez vouloir télécharger une page web. Peut-être que vous voulez conserver une copie locale d'une page que vous trouvez utile ou intéressante. Ou peut-être que vous travaillez sur un projet de développement web et que vous avez besoin de télécharger des pages pour tester votre code. Quelle que soit la raison, il est utile de savoir comment le faire en utilisant C#.

## Comment faire

Tout d'abord, vous aurez besoin de créer un projet dans un environnement de développement intégré (IDE) comme Visual Studio. Assurez-vous d'ajouter les références appropriées à votre projet pour pouvoir accéder aux classes et méthodes nécessaires pour télécharger une page.

Ensuite, vous pouvez utiliser la classe `WebRequest` pour créer une demande de téléchargement. Vous devrez fournir une URL valide pour la page que vous souhaitez télécharger. Vous pouvez également spécifier des en-têtes de demande et des paramètres de demande si nécessaire.

Voici un exemple de code pour télécharger une page et afficher son contenu:

```C#
var url = "https://www.example.com";
var request = WebRequest.Create(url);
using (var response = request.GetResponse())
{
    using (var stream = response.GetResponseStream())
    {
        using (var reader = new StreamReader(stream))
        {
            var html = reader.ReadToEnd();
            Console.WriteLine(html);
        }
    }
}
```

En exécutant ce code, vous devriez voir le contenu de la page imprimé dans la console. Vous pouvez également enregistrer ce contenu dans un fichier si vous le souhaitez.

## Plongez plus profondément

Télécharger une page web peut sembler simple, mais il y a en fait beaucoup plus de choses à considérer en termes de performance et d'optimisation. Vous pouvez utiliser des méthodes de compression pour réduire la taille de le chargement, gérer les cookies pour les demandes authentifiées et utiliser des bibliothèques tierces pour un contrôle plus granulaire sur la demande de téléchargement.

De plus, vous voudrez peut-être prendre en compte la sécurité en vérifiant que la page que vous téléchargez est fiable et sûre.

## Voir aussi

- [Références de la classe WebRequest](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.webrequest)
- [Tutoriel sur les requêtes web en C#](https://www.c-sharpcorner.com/article/working-with-web-requests/)
- [Utiliser la classe WebRequest en mode asynchrone](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.webrequest.getresponseasync)