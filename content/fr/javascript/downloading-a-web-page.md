---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Télécharger une page Web consiste à récupérer le contenu HTML de celle-ci depuis un serveur Web. Les programmeurs font cela pour obtenir des données à partir de sites Web et les manipuler en fonction de leurs besoins.

## Comment faire :

Télécharger une page Web en Javascript est assez simple, surtout avec l'aide de bibliothèques comme 'axios'. Voici comment vous pouvez la faire.

```Javascript
const axios = require('axios');

axios.get('https://www.votresite.com')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error(error);
  });
```
Dans cet exemple, l'URL 'https://www.votresite.com' est la page Web que vous voulez télécharger. La fonction `.then()` est exécutée lorsque le téléchargement est terminé et affiche le contenu de la page. Si une erreur survient, elle est capturée et affichée grâce à la fonction `.catch()`.

## Plongée en profondeur :

Le téléchargement de pages Web a été facilité grâce à l'avènement des bibliothèques d'extraction de données Web telle que 'axios'. Avant cela, les développeurs devaient souvent écrire manuellement cette fonctionnalité à l'aide de méthodes natives en Javascript, comme 'XMLHttpRequest'.

Il existe également des alternatives à 'axios', comme 'fetch', qui est intégré aux navigateurs modernes. Cependant, 'fetch' ne supporte pas l'ancienne version du navigateur.

Quant à l'implémentation, il est bon de savoir que 'axios' renvoie une promesse. Cela signifie que vous pouvez utiliser `.then()` pour attendre la fin du téléchargement, ou `async/await` pour rendre votre code plus lisible.

## Voir aussi :

1. [Documentation axios](https://github.com/axios/axios)
2. [API Fetch](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
3. [Guide Async/Await](https://developer.mozilla.org/fr/docs/Learn/JavaScript/Asynchronous/Async_await)

Voilà, avec cela vous devriez être en mesure de télécharger une page Web en JavaScript. Bonne programmation!