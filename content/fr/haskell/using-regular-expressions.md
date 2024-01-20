---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Utiliser des expressions régulières (regex) permet de chercher et manipuler du texte selon un pattern défini. Les programmeurs s'en servent pour faciliter le traitement de chaînes de caractères, de la validation de données au parsing complexe.

## How to:
Haskell utilise le paquet `regex-tdfa` pour les regex. Voici un exemple simple :

```Haskell
import Text.Regex.TDFA

main :: IO ()
main = do
    let text = "Je programme en Haskell depuis 2022"
    let pattern = "[0-9]+"
    print (text =~ pattern :: String)
```

Sortie:

```
"2022"
```

Pour remplacer du texte:

```Haskell
import Text.Regex.TDFA

main :: IO ()
main = do
    let text = "Je programme en Haskell depuis 2022"
    let pattern = "2022"
    let replacement = "2023"
    print (text =~ pattern :: String)
    print (subRegex (mkRegex pattern) text replacement)
```

Sortie:

```
"2022"
"Je programme en Haskell depuis 2023"
```

## Deep Dive
Les regex en Haskell reposent sur les paquets tels que `regex-tdfa`, inspirés par les travaux de Stephen Kleene dans les années 1950. Contrairement à Perl ou Python, leur utilisation en Haskell n'est pas intégrée directement dans le langage, nécessitant ces bibliothèques externes. Des alternatives incluent les matchings de motifs intégrés dans le langage ou des parsers comme Parsec. Les regex sont gourmands en performance ; Haskell permet l'optimisation via la compilation de regex.

## See Also
- [Hackage - regex-tdfa package](https://hackage.haskell.org/package/regex-tdfa)
- [Learn You a Haskell for Great Good! - Understanding monads](http://learnyouahaskell.com/a-fistful-of-monads)