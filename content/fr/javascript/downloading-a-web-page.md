---
title:                "Télécharger une page web"
html_title:           "Javascript: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Qu'est-ce que le téléchargement d'une page web et pourquoi les programmeurs le font-ils?

Le téléchargement d'une page web est le processus par lequel un navigateur récupère une page web à partir d'un serveur, puis l'affiche sur votre écran. Les programmeurs utilisent souvent cette technique pour accéder à des données dynamiques à partir de sites web et les intégrer dans leurs propres applications.

# Comment faire:

### Télécharger une page web en utilisant Javascript

```Javascript
// Créez une variable pour stocker l'URL de la page web à télécharger
var url = "https://www.example.com";

// Utilisez l'objet XMLHttpRequest pour créer une requête HTTP
var xhr = new XMLHttpRequest();

// Utilisez la méthode open() pour spécifier le type de requête et l'URL à télécharger
xhr.open("GET", url, true);

// Utilisez la méthode send() pour envoyer la requête
xhr.send();

// Utilisez la méthode onload pour spécifier une fonction à exécuter lorsque la réponse est reçue
xhr.onload = function () {
  // Utilisez la propriété responseText pour accéder au contenu de la page web téléchargée
  var page = xhr.responseText;
  console.log(page); // Affiche le contenu de la page dans la console
}
```

### Télécharger une page web en utilisant des librairies externes

Il existe également des librairies externes telles que jQuery et axios qui facilitent le processus de téléchargement d'une page web en fournissant des méthodes simplifiées pour effectuer des requêtes HTTP et gérer les réponses.

Voici un exemple en utilisant jQuery:

```Javascript
// Utilisez la méthode get() pour télécharger une page web
$.get("https://www.example.com", function(data) {
  console.log(data); // Affiche le contenu de la page dans la console
});
```

Et voici un exemple en utilisant axios:

```Javascript
// Utilisez la méthode get() pour télécharger une page web
axios.get("https://www.example.com")
  .then(function(response) {
    console.log(response.data); // Affiche le contenu de la page dans la console
  })
  .catch(function(error) {
    console.log(error);
  });
```

# Plongée en profondeur:

### Contexte historique

Auparavant, le téléchargement de pages web était principalement utilisé pour l'accès à des données statiques telles que des images ou des fichiers CSS. Mais avec les avancées technologiques, le téléchargement de pages web est devenu un moyen commun pour les programmeurs de créer des applications web dynamiques.

### Alternatives

Bien qu'il soit couramment utilisé en programmation web, le téléchargement de pages web n'est pas la seule solution pour accéder à des données sur des sites externes. D'autres alternatives incluent le "web scraping" (l'extraction d'informations à partir de pages web) et l'utilisation d'API (interfaces de programmation d'application) fournies par les sites web eux-mêmes.

### Détails de mise en œuvre

Le téléchargement d'une page web peut être réalisé en utilisant différentes méthodes, mais la plupart d'entre elles impliquent l'utilisation d'une requête HTTP pour récupérer les données. Il est important de vérifier que vous avez le droit de télécharger les données à partir du site web que vous ciblez, car cela peut être considéré comme une violation des conditions d'utilisation ou des droits d'auteur.

# Voir aussi:

- [XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest) sur MDN
- [jQuery](https://jquery.com/) officiel
- [axios](https://github.com/axios/axios) sur GitHub