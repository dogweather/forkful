---
title:                "Analyse de l'html"
html_title:           "Fish Shell: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

# Quoi et Pourquoi?

Parsing HTML, or extracting data from HTML code, is a useful skill for programmers who want to scrape websites or work with web data. Parsing HTML helps simplify and organize the data for easier analysis and manipulation.

# Comment faire:

Voici comment parser HTML en utilisant Fish Shell:

```
set html (curl -s <url>) # Récupère le code HTML à partir de l'URL spécifiée et le stocke dans la variable html
set data (string split -r "</?tag>" $html) # Sépare le code HTML en morceaux en utilisant des balises
for line in $data # Parcourt chaque morceau
  echo $line # Affiche le morceau en console
end
```

Voici un exemple de sortie pour un site avec des balises 'h1' et 'p':

```
1. Site web
2. Ceci est un paragraphe
```

# Plongée en profondeur:

Le parsing HTML est une technique couramment utilisée dans le traitement de données web et le scraping de sites web. Cela a été particulièrement utile avant l'avènement des API qui permettent un accès plus structuré aux données. D'autres langages de programmation comme Python ont également des méthodes intégrées spécialement pour parser HTML, mais Fish Shell peut également être utilisé via l'utilisation des commandes système telles que 'curl' et 'string'.

# Références utiles:

- Pour en savoir plus sur le parsing HTML avec Fish Shell, consultez la documentation officielle: https://fishshell.com/docs/current/cmds/string.html#string-split
- Apprenez-en plus sur les fonctionnalités de Fish Shell sur leur site web: https://fishshell.com/
- Pour une alternative à l'utilisation de Fish Shell pour le parsing HTML, consultez: https://wiki.python.org/moin/HTMLParsing