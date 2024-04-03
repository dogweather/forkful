---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:33.104491-07:00
description: "Travailler avec JSON en Elm consiste \xE0 d\xE9coder des donn\xE9es\
  \ JSON en types Elm et \xE0 encoder des valeurs Elm en retour en JSON. Ce processus\
  \ est crucial pour\u2026"
lastmod: '2024-03-13T22:44:57.713268-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec JSON en Elm consiste \xE0 d\xE9coder des donn\xE9es JSON\
  \ en types Elm et \xE0 encoder des valeurs Elm en retour en JSON."
title: Travailler avec JSON
weight: 38
---

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
