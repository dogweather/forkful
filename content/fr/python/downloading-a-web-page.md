---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Télécharger une Page Web avec Python

## Quoi & Pourquoi?

Le téléchargement d'une page Web signifie la récupération du code HTML derrière une page Web à partir d'un serveur et son stockage dans votre appareil. Les programmeurs le font pour analyser ce contenu, extraire des données importantes (appelé scrapage de données) ou tester des performances de site Web.

## Comment faire:

Téléchargeons une page Web en utilisant la bibliothèque `requests`.

```Python
    import requests

    url = 'https://www.example.com'
    response = requests.get(url)

    # afficher le code de status HTTP
    print(response.status_code)

    # afficher le contenu de la page
    print(response.text)
```

Lors de l'exécution de ce script, vous verrez d'abord un code de statut HTTP (par exemple, `200` pour une demande réussie), puis le contenu HTML de la page.

## Approfondissement

Historiquement, le téléchargement d'une page Web était un processus plus manuel impliquant l'ouverture d'un socket à un serveur, l'envoi d'une requête GET et le décodage de la réponse qui était souvent en format binaire.

Outre `requests`, il existe d'autres bibliothèques pour télécharger une page Web en Python, dont `urllib` et `httplib`. Cependant, `requests` est largement reconnue pour son API simple et conviviale.

Lorsque vous téléchargez une page Web avec `requests`, les éléments sont automatiquement décodés en textes à partir des octets. Dans le cas des sites Web encodés différemment, `requests` utilise l'encodage spécifié dans les en-têtes HTTP pour décoder le contenu de la page.

## Voir Aussi

- Documentation requests: [https://docs.python-requests.org/en/latest/](https://docs.python-requests.org/en/latest/)
- Tutoriel sur le scrapage de données avec Python: [https://realpython.com/beautiful-soup-web-scraper-python/](https://realpython.com/beautiful-soup-web-scraper-python/)
- Un guide pour travailler avec des sockets en Python: [https://realpython.com/python-sockets/](https://realpython.com/python-sockets/)