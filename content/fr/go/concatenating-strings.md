---
title:                "Go: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi
Dans la programmation Go, il est très courant de devoir manipuler des chaînes de caractères et de devoir les concaténer. La concaténation de chaînes peut être utile lors de la création de messages personnalisés, l'affichage de données utilisateur ou la construction de requêtes HTTP. Dans cet article, nous allons découvrir comment concaténer des chaînes en utilisant Go.

## Comment faire
Il existe plusieurs façons de concaténer des chaînes en Go. La première méthode consiste à utiliser l'opérateur "+" pour combiner deux chaînes ensemble. Voyons un exemple de code:

```Go
prenom := "Jean"
nom := "Dupont"
concatene := prenom + nom
```

Dans cet exemple, nous définissons deux chaînes distinctes pour le prénom et le nom, puis nous utilisons l'opérateur "+" pour les concaténer en une seule chaîne. Si nous imprimons cette variable "concatene", nous obtenons "JeanDupont" en sortie.

## Plongée en profondeur
Il est important de noter que l'utilisation de "+" pour concaténer des chaînes peut être très coûteuse en termes de performance, surtout lorsque vous travaillez avec de grandes chaînes de caractères. Une meilleure approche consiste à utiliser le type de données "bytes.Buffer". Voyons un exemple:

```Go
var concatene bytes.Buffer
concatene.WriteString("Bonjour")
concatene.WriteString("tout le monde")
fmt.Println(concatene.String())
```

Avec cette méthode, nous créons un "Buffer" et nous utilisons la méthode "WriteString" pour ajouter des chaînes à celui-ci. Enfin, nous imprimons la chaîne concaténée en utilisant la méthode "String()". Cette approche est beaucoup plus efficace en termes de performances et est recommandée lors de la manipulation de grandes chaînes.

# Voir aussi
Si vous souhaitez en savoir plus sur les méthodes de manipulation de chaînes en Go, n'hésitez pas à consulter les ressources suivantes:
- La documentation officielle sur les chaînes en Go: https://golang.org/pkg/strings/
- Un tutoriel sur les différents types de données en Go, y compris les chaînes: https://golangbot.com/types/
- Un article sur les performances de la concaténation de chaînes en Go: https://www.calhoun.io/concatenating-strings-in-go/