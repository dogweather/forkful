---
title:                "Haskell: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hvorfor

YAML er et strukturert dataformat som er populært blant utviklere for å lagre og overføre data. Det er enkelt å lese og skrive, og er spesielt nyttig for konfigurasjonsfiler. I denne bloggposten skal vi se nærmere på hvordan du kan arbeide med YAML i Haskell.

# Hvordan

For å begynne å arbeide med YAML i Haskell, må vi først importere noen nødvendige moduler. Vi vil bruke pakken `yaml` til å håndtere YAML-data, samt `Data.Yaml` modulen fra denne pakken.

```Haskell
import Text.Yaml
import Data.Yaml
```

## Lesing av YAML-data

La oss si at vi har en YAML-konfigurasjonsfil kalt `config.yaml` med følgende innhold:

```yaml
database:
    host: localhost
    port: 5432
    username: admin
    password: secret
```

Vi kan enkelt lese inn denne filen ved å bruke `decodeFileEither` funksjonen fra `Data.Yaml` modulen.

```Haskell
data Config = Config
    { dbHost :: String
    , dbPort :: Int
    , dbUsername :: String
    , dbPassword :: String
    } deriving (Show, Eq)

instance FromYaml Config where
    parseYaml = withMap "Config" $ \obj -> do
        host <- obj .: "database" >>= (.: "host")
        port <- obj .: "database" >>= (.: "port")
        username <- obj .: "database" >>= (.: "username")
        password <- obj .: "database" >>= (.: "password")
        return $ Config host port username password

main :: IO ()
main = do
    res <- decodeFileEither "config.yaml" :: IO (Either ParseException Config)
    case res of
        Left err -> print err
        Right config -> print config
```

Output:

```
Right (Config {dbHost = "localhost", dbPort = 5432, dbUsername = "admin", dbPassword = "secret"})
```

## Skriving av YAML-data

Vi kan også skrive YAML-data i Haskell ved hjelp av `encodeFile` funksjonen. La oss si at vi ønsker å lage en YAML-fil med noen brukerdata som følger:

```Haskell
data User = User
    { name :: String
    , age :: Int
    , email :: String
    } deriving (Show, Eq)

instance ToYaml User where
    toYaml user = mapping
        [ "name" .= name user
        , "age" .= age user
        , "email" .= email user
        ]

main :: IO ()
main = encodeFile "user.yaml" (User "John" 30 "john@example.com")
```

Dette vil lage en fil kalt `user.yaml` med følgende innhold:

```yaml
name: John
age: 30
email: john@example.com
```

# Dype Dykk

For å lære mer om hvordan man kan arbeide med YAML i Haskell, kan du sjekke ut dokumentasjonen til `yaml` pakken [her](http://hackage.haskell.org/package/yaml) og til `Data.Yaml` modulen [her](http://hackage.haskell.org/package/yaml-0.11.0.0/docs/Data-Yaml.html).

# Se også

- [YAML.org](https://yaml.org/)
- [Haskell.org](https://www.haskell.org/)