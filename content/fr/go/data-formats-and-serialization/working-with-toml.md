---
aliases:
- /fr/go/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:31.645765-07:00
description: "TOML (Tom's Obvious, Minimal Language ou Langage Minimal \xC9vident\
  \ de Tom) est un format de fichier de configuration qui est facile \xE0 lire gr\xE2\
  ce \xE0 sa\u2026"
lastmod: 2024-02-18 23:09:08.264319
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language ou Langage Minimal \xC9vident de Tom)\
  \ est un format de fichier de configuration qui est facile \xE0 lire gr\xE2ce \xE0\
  \ sa\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

TOML (Tom's Obvious, Minimal Language ou Langage Minimal Évident de Tom) est un format de fichier de configuration qui est facile à lire grâce à sa syntaxe simple. Les programmeurs utilisent TOML pour configurer les paramètres d'application et les dépendances en raison de sa clarté et de son mappage direct aux structures de données, ce qui en fait un choix populaire dans de nombreux projets Go pour la mise en place et la gestion des configurations.

## Comment faire :

Pour commencer à travailler avec TOML en Go, vous devez d'abord inclure une bibliothèque capable d'analyser les fichiers TOML puisque la bibliothèque standard de Go ne prend pas nativement en charge TOML. Le package `BurntSushi/toml` est un choix populaire pour cela. Tout d'abord, assurez-vous de l'installer :

```bash
go get github.com/BurntSushi/toml
```

Voici un exemple simple de son utilisation. Supposons que vous avez un fichier de configuration nommé `config.toml` avec le contenu suivant :

```toml
title = "Exemple TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Maintenant, vous devez créer une structure Go qui reflète la structure TOML :

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Titre : %s\n", config.Title)
    fmt.Printf("Serveur de base de données : %s\n", config.Database.Server)
}
```

Exemple de sortie :

```
Titre : Exemple TOML
Serveur de base de données : 192.168.1.1
```

## Plongée profonde

TOML a été créé par Tom Preston-Werner, l'un des cofondateurs de GitHub, pour offrir un format de fichier de configuration simple qui peut être facilement mappé sur une table de hachage et compris d'un seul coup d'œil sans connaissance préalable du format. Il se contraste avec JSON ou YAML, qui, tout en étant également largement utilisés, peuvent être moins conviviaux pour les fichiers de configuration en raison des problèmes d'accolades, de guillemets et d'indentation.

Le package `BurntSushi/toml` en Go est une bibliothèque robuste qui permet non seulement le décodage mais aussi l'encodage de fichiers TOML, ce qui en fait un choix polyvalent pour les applications qui doivent lire et écrire des fichiers de configuration dans ce format. Cependant, il convient de noter qu'avec l'avancement des technologies et l'introduction de nouvelles versions de Go, des alternatives telles que `pelletier/go-toml` ont émergé, offrant une meilleure performance et des fonctionnalités supplémentaires comme la manipulation d'arbre et le support de requêtes.

Bien que TOML soit un excellent choix pour de nombreuses applications, selon la complexité de la configuration de l'application et les préférences personnelles ou de l'équipe, d'autres formats comme YAML ou JSON pourraient être mieux adaptés, surtout si la configuration nécessite des structures de données plus complexes que la nature verbeuse de TOML pourrait ne pas capturer élégamment. Néanmoins, pour des configurations simples, lisibles et facilement modifiables, TOML, associé au système de typage fort de Go et aux bibliothèques mentionnées ci-dessus, est un choix excellent.
