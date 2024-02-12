---
title:                "Travailler avec JSON"
aliases:
- fr/elm/working-with-json.md
date:                  2024-02-03T19:22:33.104491-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec JSON en Elm consiste à décoder des données JSON en types Elm et à encoder des valeurs Elm en retour en JSON. Ce processus est crucial pour les applications web afin d'interagir avec des API et des sources de données externes, permettant un échange de données fluide entre le client (Elm) et les serveurs ou d'autres services.

## Comment faire :

Elm traite la gestion de JSON avec explicité et sécurité, en utilisant principalement les modules `Json.Decode` et `Json.Encode`. Pour commencer à travailler avec JSON, vous devez d'abord définir un décodeur pour votre type de données. Supposons que nous traitons avec un simple objet de profil utilisateur.

Tout d'abord, définissez votre type Elm :

```elm
type alias ProfilUtilisateur = 
    { id : Int
    , nom : String
    , email : String
    }
```

### Décoder JSON en Elm

Pour décoder une chaîne JSON en type `ProfilUtilisateur`, créez un décodeur :

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

decodeurProfilUtilisateur : Decoder ProfilUtilisateur
decodeurProfilUtilisateur =
    map3 ProfilUtilisateur
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Pour décoder un objet JSON :

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decode : Result String ProfilUtilisateur
decode =
    decodeString decodeurProfilUtilisateur jsonString

{- Exemple de sortie :
Result.Ok { id = 1, nom = "John Doe", email = "john@example.com" }
-}
```

### Encoder Elm en JSON

Pour encoder une valeur Elm en retour en JSON, utilisez le module `Json.Encode`.

```elm
import Json.Encode exposing (object, int, string)

encoderProfilUtilisateur : ProfilUtilisateur -> String
encoderProfilUtilisateur profilUtilisateur =
    object
        [ ("id", int profilUtilisateur.id)
        , ("name", string profilUtilisateur.nom)
        , ("email", string profilUtilisateur.email)
        ]
        |> Json.Encode.encode 0

{- 
Usage:
encoderProfilUtilisateur { id = 1, nom = "John Doe", email = "john@example.com" }

Exemple de sortie :
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Bibliothèques tierces

Des packages Elm comme `elm-json-decode-pipeline` peuvent simplifier la création de décodeurs en utilisant un style de pipeline, ce qui est particulièrement pratique pour décoder des objets complexes.

Tout d'abord, ajoutez la bibliothèque à votre projet :

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Ensuite, vous pouvez simplifier la définition du décodeur comme suit :

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

decodeurProfilUtilisateur : Decoder ProfilUtilisateur
decodeurProfilUtilisateur =
    decode ProfilUtilisateur
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Utilisez ce décodeur comme avant avec decodeString pour décoder les chaînes JSON. -}
```

Cette approche simplifie le décodeur, rendant le code plus propre et plus facile à maintenir, surtout à mesure que les structures de données deviennent plus complexes.
