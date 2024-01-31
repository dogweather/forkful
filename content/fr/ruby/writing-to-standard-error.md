---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"Quoi et Pourquoi ?"

Écrire sur l'erreur standard (`stderr`) permet d'afficher des messages d'erreur sans les mêler à la sortie standard (`stdout`). Les programmeurs l'utilisent pour signaler des problèmes tout en gardant la sortie des données propre et scriptable.

## How to:
"Comment faire :"

```Ruby
# Ecrire un message d'erreur simple sur stderr
$stderr.puts "Erreur: fichier non trouvé"

# Vous pouvez aussi utiliser STDERR directement
STDERR.puts "Attention: opération non valide"

# Capture de l'erreur dans une variable avant de l'écrire sur stderr
erreur_msg = "Échec de la connexion à la base de données"
$stderr.puts erreur_msg

# Rediriger stderr vers un fichier
$stderr.reopen("erreurs.log", "w")
$stderr.puts "Toutes les erreurs seront écrites ici"
```
Output attendu sur votre terminal pour les deux premières lignes :
```
Erreur: fichier non trouvé
Attention: opération non valide
```

## Deep Dive
"Plongée en détail"

Historiquement, la séparation de `stdout` et `stderr` vient des premiers jours du système UNIX, afin de permettre aux utilisateurs de distinguer les données du programme des messages d'erreur. Pourquoi ne pas simplement utiliser `puts` ou `print` ? Ces méthodes écrivent sur `stdout` par défaut, ce qui n'est pas idéale pour les messages d'erreur, surtout lorsqu'on pipe ou redirige les sorties dans les shells Unix/Linux. En Ruby, l'objet `$stderr` est une instance globale de `IO` spécialisée pour traiter les erreurs, tandis que `STDERR` est une constante référençant le même objet.

## See Also
"Voir Aussi"

- Documentation Ruby IO: https://ruby-doc.org/core-3.1.2/IO.html
- Explication de STDOUT et STDERR sur UNIX: https://www.jstor.org/stable/unixsystem.odos?seq=5#metadata_info_tab_contents
- Guide approfondi sur la redirection en Ruby: https://www.honeybadger.io/blog/ruby-stdout-stderr/
