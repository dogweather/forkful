---
title:                "Envoyer une demande http avec une authentification de base"
html_title:           "Go: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Envoyer une requête HTTP avec une authentification de base est un moyen pour les programmeurs de se connecter à des API ou des sites Web sécurisés en fournissant des informations d'identification de base. Cela permet de s'assurer que seules les personnes autorisées peuvent accéder à ces données sensibles.

## Comment faire:
Voici un exemple de code en Go pour envoyer une requête HTTP avec une authentification de base:

```
func main() {
  // Déclarer les informations d'identification de base
  username := "foo"
  password := "bar"

  // Créer une requête avec l'URL et les informations d'identification
  req, err := http.NewRequest("GET", "https://exemple.com/api/data", nil)
  req.SetBasicAuth(username, password)

  // Envoyer la requête et récupérer la réponse
  client := &http.Client{}
  resp, err := client.Do(req)

  if err != nil {
    fmt.Println(err)
    return
  }

  // Afficher le code de réponse et le corps de la réponse
  fmt.Println(resp.Status)
  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  fmt.Println(string(body))
}
```

L'exemple de code ci-dessus utilise la fonction `NewRequest` de la bibliothèque standard `net/http` pour créer une requête avec l'URL cible. Ensuite, la fonction `SetBasicAuth` est utilisée pour fournir les informations d'identification de base. Enfin, la requête est envoyée avec `client.Do(req)`, et la réponse est récupérée et affichée. Il est important de vérifier les erreurs lors de l'envoi et de la récupération de la réponse pour assurer un fonctionnement correct de la requête.

## Plongée en profondeur:
L'authentification de base a été initialement définie dans le RFC 2617, publié en 1999 en réponse à la nécessité de créer un moyen simple d'authentifier les utilisateurs sur des sites Web. Cependant, en raison des failles de sécurité importantes, il est maintenant recommandé d'utiliser des méthodes d'authentification plus robustes telles que l'authentification par OAuth ou JWT.

L'utilisation de l'authentification de base peut également varier en fonction du type de serveur auquel vous accédez. Par exemple, certains serveurs peuvent utiliser une méthode alternative pour fournir les informations d'identification au lieu d'utiliser les en-têtes d'autorisation standard.

Enfin, lors de l'implémentation de l'authentification de base, il est important de s'assurer que les informations d'identification sont transmises de manière sécurisée, par exemple en utilisant une connexion HTTPS.

## Voir aussi:
- [Documentation officielle sur l'authentification de base en Go](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
- [Le RFC 2617 sur l'authentification de base](https://tools.ietf.org/html/rfc2617)
- [Comparaison entre l'authentification de base et l'authentification par jeton](https://www.redhat.com/en/blog/http-basic-access-authentication-and-legacy-application-modernization)