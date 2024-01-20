---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?

Télécharger une page web, c'est récupérer et stocker localement son contenu HTML. Les programmeurs le font pour parser le contenu, extraire des informations, ou tester leur propre code. 

## Comment faire :

Jetez un œil à ce code Haskell simple pour télécharger une page web:

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    response <- httpBS "http://example.com"
    C.putStrLn $ getResponseBody response
```

En lançant ce script, vous devriez voir le code HTML de "http://example.com" sur la console.

```Haskell
<!doctype html>
<html>
<head>
    ...
</html>
```

## Exploration en profondeur

Le téléchargement de pages Web a commencé avec l'émergence de HTTP en 1991, simplifié au fil des décennies avec diverses librairies dans de nombreux langages. En Haskell, on utilise souvent la librairie `Network.HTTP.Simple`. Il existe aussi d'autres alternatives, par exemple `http-client` pour des cas d'utilisation plus avancés.

Dans l'exemple précédent, `httpBS` fait une demande GET au lien fourni, le résultat est récupéré et imprimé dans la console. Les réponses HTTP sont manipulées comme des `ByteString`, un type de données efficace pour manipuler des octets bruts en Haskell.

## Voir aussi

1. [Tutoriel Haskell](https://learnxinyminutes.com/docs/haskell/)
2. [Documentation de Network.HTTP.Simple](https://hackage.haskell.org/package/http-conduit-2.3.7.3/docs/Network-HTTP-Simple.html)
3. [ByteString en Haskell](https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html)

Ces ressources vous aideront à approfondir vos connaissances et à comprendre mieux l'interaction entre Haskell et le web.