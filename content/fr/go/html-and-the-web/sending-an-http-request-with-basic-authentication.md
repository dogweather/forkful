---
aliases:
- /fr/go/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:02.431175-07:00
description: "Envoyer une requ\xEAte HTTP avec authentification de base en Go implique\
  \ d'ajouter un en-t\xEAte d'autorisation \xE0 votre requ\xEAte qui comprend un nom\u2026"
lastmod: 2024-02-18 23:09:08.233072
model: gpt-4-0125-preview
summary: "Envoyer une requ\xEAte HTTP avec authentification de base en Go implique\
  \ d'ajouter un en-t\xEAte d'autorisation \xE0 votre requ\xEAte qui comprend un nom\u2026"
title: "Envoyer une requ\xEAte HTTP avec une authentification de base"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Envoyer une requête HTTP avec authentification de base en Go implique d'ajouter un en-tête d'autorisation à votre requête qui comprend un nom d'utilisateur et un mot de passe sous forme de chaîne encodée en Base64. Les programmeurs utilisent cette méthode pour accéder à des ressources nécessitant une vérification de l'utilisateur, garantissant que leurs applications peuvent interagir de manière sécurisée avec des services sur le web.

## Comment faire :

Pour effectuer une requête HTTP avec authentification de base en Go, vous devez préparer vos en-têtes de requête pour inclure le champ `Authorization`, rempli avec vos identifiants dans le format correct. Ci-dessous, un exemple qui montre comment effectuer une requête GET vers un point d'API qui nécessite une authentification de base :

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // Encoder les identifiants
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Définir l'en-tête d'autorisation
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Statut de la réponse :", resp.Status)
}
```

Exécuter ce code enverra une requête GET à l'URL spécifiée avec l'en-tête d'autorisation nécessaire. La sortie ressemblera à quelque chose comme ceci, en fonction de votre point de terminaison et service :

```
Statut de la réponse : 200 OK
```

## Approfondissement

L'authentification de base dans les requêtes HTTP est une méthode largement prise en charge pour renforcer les contrôles d'accès aux ressources web. Elle envoie simplement un nom d'utilisateur et un mot de passe avec chaque requête, ce qui la rend facile à mettre en œuvre, mais ce n'est pas la méthode la plus sécurisée disponible. Un inconvénient majeur est que, sauf utilisation conjointe avec SSL/TLS, les identifiants sont envoyés en clair (puisque Base64 est facilement décodé), ce qui peut potentiellement exposer des informations sensibles aux attaques de type man-in-the-middle.

En Go, l'envoi de ces requêtes implique de manipuler directement l'en-tête `Authorization`. Alors que la bibliothèque standard de Go (`net/http`) fournit des primitives puissantes pour traiter les communications HTTP(s), elle est relativement bas niveau, obligeant les développeurs à gérer manuellement divers aspects du traitement des requêtes/réponses HTTP. Cela offre beaucoup de flexibilité aux programmeurs, mais cela signifie également qu'il faut prêter une attention particulière aux implications en matière de sécurité, au codage et à la gestion correcte des en-têtes.

Pour les applications nécessitant une sécurité plus élevée, des systèmes d'authentification plus avancés tels que OAuth2 ou JWT (Jetons Web JSON) devraient être envisagés. Ces approches fournissent des fonctionnalités de sécurité plus robustes et sont largement supportées à travers les API et services modernes. L'écosystème en expansion de Go comprend de nombreuses bibliothèques et outils (tels que `golang.org/x/oauth2`, parmi d'autres) pour faciliter ces méthodes d'authentification plus sécurisées, rendant plus facile pour les développeurs de mettre en œuvre des mécanismes d'autorisation sûrs, efficaces et modernes dans leurs applications.
