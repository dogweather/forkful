---
title:                "PHP: Envoi d'une demande http avec authentification de base"
simple_title:         "Envoi d'une demande http avec authentification de base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans les détails de l'envoi d'une requête HTTP avec une authentification de base en PHP, il est important de comprendre pourquoi une telle action serait nécessaire. Envoyer une requête HTTP avec une authentification de base est utile lors de l'utilisation d'une API qui nécessite une authentification pour accéder à ses ressources. Cela peut être dans le cadre d'une application web ou mobile qui communique avec un serveur distant.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en PHP, il faut utiliser la fonction cURL intégrée. Voici un exemple de code PHP avec des valeurs d'exemple pour l'URL, le nom d'utilisateur et le mot de passe :

```PHP
$url = "https://exemple.com/api";
$username = "utilisateur";
$password = "mot de passe";

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$output = curl_exec($ch);
curl_close($ch);
```

La fonction cURL envoie une requête avec l'URL spécifiée, ainsi que les informations d'authentification de base incluses dans l'en-tête de la requête. L'option "CURLOPT_RETURNTRANSFER" est utilisée pour récupérer la réponse du serveur et la stocker dans la variable $output. Vous pouvez ensuite utiliser cette réponse pour traiter les données renvoyées par l'API.

Voici un exemple de réponse de l'API :

```
{
   "message": "Authentification réussie",
   "user_id": "1234"
}
```

## Plongée en profondeur

Maintenant que nous avons vu comment envoyer une requête HTTP avec une authentification de base en PHP avec la fonction cURL, plongeons un peu plus loin dans le processus. La première chose que nous devons faire est d'établir une connexion avec l'API en utilisant la fonction "curl_init()". Ensuite, nous pouvons définir certaines options pour personnaliser notre requête, telles que l'URL, les informations d'authentification et l'option "CURLOPT_RETURNTRANSFER" pour récupérer la réponse.

Une fois que nous avons envoyé notre requête, il est important de vérifier le code d'état de la réponse pour s'assurer que tout s'est bien passé. En général, si le code d'état est "200", cela signifie que la connexion et la requête ont été effectuées avec succès. Si un autre code d'état est retourné, cela indique qu'il y a eu un problème avec la requête et il faut alors vérifier les détails de la réponse pour en déterminer la cause.

Il est également important de comprendre que l'authentification de base n'est pas la méthode la plus sécurisée pour envoyer des informations d'identification, car le nom d'utilisateur et le mot de passe sont envoyés en texte clair. Il est préférable d'utiliser une méthode plus sécurisée, comme l'authentification OAuth, si possible.

## Voir aussi

- [Documentation officielle de PHP sur la fonction cURL](https://www.php.net/manual/fr/book.curl.php)
- [Tutoriel sur l'authentification de base avec cURL en PHP](https://www.cloudways.com/blog/php-curl-tutorial/)
- [Documentation sur les codes d'état HTTP](https://www.restapitutorial.com/httpstatuscodes.html)
- [Tutoriel sur l'authentification sécurisée avec OAuth en PHP](https://www.phpflow.com/php/oauth-2-0-for-google-api-without-any-library/)