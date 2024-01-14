---
title:                "Haskell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi Travailler avec JSON en Haskell?

JSON (JavaScript Object Notation) est un format de données très couramment utilisé dans le monde de la programmation. En travaillant avec JSON en Haskell, vous serez capable de manipuler facilement des données structurées et de les utiliser dans vos projets. De plus, cette combinaison vous permettra de bénéficier des avantages de la sécurité et de la stabilité du langage Haskell.

## Comment faire?

Pour travailler avec JSON en Haskell, il vous faut d'abord importer le module `Data.Aeson` dans votre code. Ensuite, vous pourrez utiliser la fonction `decode` pour transformer une chaîne de caractères JSON en un type de données Haskell. Vous pouvez également utiliser la fonction `encode` pour convertir un type de données Haskell en une chaîne de caractères JSON.

Voici un exemple de code pour décoder et encoder un type de données "Personne" avec un nom et un âge :

```Haskell
import Data.Aeson

-- Définition du type de données
data Personne = Personne { nom :: String, age :: Int } deriving Show

-- Conversion en JSON
instance FromJSON Personne
instance ToJSON Personne

-- Décodage d'une chaîne de caractères JSON en une Personne
decodePersonne :: Maybe Personne
decodePersonne = decode "{\"nom\":\"Marie\", \"age\":25}"

-- Encodage d'une Personne en une chaîne de caractères JSON
encodePersonne :: Maybe String
encodePersonne = encode (Personne {nom = "Pierre", age = 32})

-- Résultats
print decodePersonne -- Just (Personne {nom = "Marie", age = 25})
print encodePersonne -- Just "{\"nom\":\"Pierre\",\"age\":32}"
```

## Plongeon Profond

Le module `Data.Aeson` contient de nombreuses fonctions utiles pour travailler avec JSON en Haskell, telles que `encodeFile` pour écrire des données dans un fichier JSON, `decodeFile` pour lire des données à partir d'un fichier JSON, et `FromJSON` et `ToJSON` pour définir des instances de conversion pour des types de données personnalisés.

Vous pouvez également utiliser des librairies externes comme `aeson-better-errors` pour gérer les erreurs lors de la conversion de données JSON en Haskell, ou `lens-aeson` pour utiliser la bibliothèque de programmation fonctionnelle Lens avec des types de données JSON.

N'hésitez pas à explorer ces options pour trouver celles qui conviennent le mieux à vos besoins et à votre style de programmation.

## Voir aussi

- [Documentation officielle de Data.Aeson en Haskell](https://hackage.haskell.org/package/aeson)
- [Tutoriel pour travailler avec JSON en Haskell](https://haskell-at-work.com/episodes/2019-06-19-work-with-json-data)
- [Bibliothèque `aeson-better-errors`](https://hackage.haskell.org/package/aeson-better-errors)
- [Bibliothèque `lens-aeson`](https://hackage.haskell.org/package/lens-aeson)