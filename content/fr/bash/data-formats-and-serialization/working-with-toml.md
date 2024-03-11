---
date: 2024-01-26 04:18:42.566770-07:00
description: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format\
  \ de s\xE9rialisation de donn\xE9es. Les programmeurs l'appr\xE9cient pour sa simplicit\xE9\
  \ et sa\u2026"
lastmod: '2024-03-11T00:14:31.947846-06:00'
model: gpt-4-0125-preview
summary: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format de\
  \ s\xE9rialisation de donn\xE9es. Les programmeurs l'appr\xE9cient pour sa simplicit\xE9\
  \ et sa\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML, abréviation de Tom's Obvious, Minimal Language, est un format de sérialisation de données. Les programmeurs l'apprécient pour sa simplicité et sa lisibilité ; il est primo pour les fichiers de configuration, des vibrations similaires à YAML mais moins encombrant que JSON pour un humain.

## Comment faire :
Premièrement, installez `toml-cli` pour jouer avec TOML dans Bash. Pratique pour lire ou modifier des fichiers TOML à la volée.

```Bash
# Installez toml-cli, notre petit assistant pour les tâches TOML
pip install toml-cli

# Imaginez que vous ayez un fichier TOML, 'config.toml'
echo -e 'title = "Demo TOML"\n\n[owner]\nowner = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Lire une valeur
toml get config.toml owner.name
# Sortie : Tom

# Définir une valeur
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Astuce pro : Utilisez des guillemets pour les clés avec des points ou des caractères funky !
```

## Plongée Profonde
Né du désamour pour les difficultés de JSON pour les humains, TOML est apparu vers 2013. Tom Preston-Werner, co-fondateur de GitHub, voulait quelque chose de super lisible. YAML et INI étaient des alternatives mais TOML, c'est comme le meilleur des deux.

Shebang, vous avez des données imbriquées et des tableaux, moins les pièges de YAML et les accolades de JSON. TOML est maintenant le choix par défaut pour la configuration dans Cargo de Rust, ce qui témoigne de son ascension dans le monde du développement. Il est piloté par une spécification, gardant les choses serrées et bien définies. Vous trouverez des analyseurs dans presque toutes les langues, le rendant largement adoptable.

## Voir Aussi
- Repo GitHub officiel de TOML : https://github.com/toml-lang/toml
- toml-cli sur PyPI : https://pypi.org/project/toml-cli/
- Comparaison des formats de sérialisation de données : https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
