---
title:                "Envoi d'une requête HTTP avec authentification de base"
aliases:
- /fr/python/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:27.847200-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP avec authentification de base, c'est transmettre un nom d'utilisateur et un mot de passe pour accéder à des ressources protégées sur le web. Les programmeurs font cela pour interagir avec les API sécurisées qui nécessitent que l'utilisateur soit identifié.

## Comment faire :
```Python
import requests
from requests.auth import HTTPBasicAuth

# Remplacez 'api_url' par l'URL de l'API que vous souhaitez utiliser
# Remplacez 'user' et 'pass' par vos véritables identifiants
api_url = "https://exemple-api.com/data"
reponse = requests.get(api_url, auth=HTTPBasicAuth('user', 'pass'))

print(reponse.status_code)
print(reponse.json())
```

Sortie échantillon:
```
200
{'détail': 'Voici les données protégées que vous avez demandées...'}
```

## Plongeon en profondeur
Historiquement, l'authentification de base HTTP a été l'un des premiers mécanismes pour sécuriser l'accès aux données via des requêtes HTTP. Aujourd'hui, bien que toujours utilisée pour sa simplicité, elle est moins sécurisée par rapport aux méthodes plus modernes comme OAuth car les identifiants sont simplement encodés en base64, une forme qui peut être décodée facilement si la connexion n'est pas sécurisée via HTTPS.

En alternative, il est courant d'utiliser des jetons d'authentification (tokens) ou d'autres mécanismes comme OAuth. Ces méthodes apportent une couche de sécurité supplémentaire en évitant que les identifiants de l'utilisateur transitent avec chaque requête.

Dans l'implémentation avec Python, le module `requests` simplifie beaucoup le processus. On utilise la classe `HTTPBasicAuth` pour passer les identifiants de manière sécurisée. Assurez-vous que l'URL est préfixée par `https` pour que la communication soit chiffrée.

## Voir également :
- Documentation sur les requêtes HTTP dans Python : https://requests.readthedocs.io/en/master/
- RFC 7617, The 'Basic' HTTP Authentication Scheme : https://tools.ietf.org/html/rfc7617
- Un guide sur l'authentification OAuth : https://oauth.net/
