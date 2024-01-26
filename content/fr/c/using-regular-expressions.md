---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Les expressions régulières sont des séquences de caractères formant un motif de recherche. Les programmeurs les utilisent pour trouver ou remplacer du texte selon des règles définies, ou pour valider des formats de données (ex. emails).

## How to: (Comment faire : )
En C, l’utilisation des expressions régulières passe par la bibliothèque `<regex.h>`. Voici un exemple de comment vérifier un format d'email :

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;
    char *email = "contact@example.com";

    // Compile l'expression régulière
    ret = regcomp(&regex, "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$", REG_ICASE);
    if (ret) {
        fprintf(stderr, "Compilation de l'expression régulière échouée\n");
        return 1;
    }

    // Exécute l'expression régulière
    ret = regexec(&regex, email, 0, NULL, 0);
    if (!ret) {
        puts("L'email est valide.");
    } else if (ret == REG_NOMATCH) {
        puts("L'email n'est pas valide.");
    } else {
        char message[100];
        regerror(ret, &regex, message, sizeof(message));
        fprintf(stderr, "Erreur d'expression régulière : %s\n", message);
        return 1;
    }

    // Libère la mémoire allouée à l'expression régulière compilée
    regfree(&regex);

    return 0;
}
```

Sortie attendue : `L'email est valide.`

## Deep Dive (Plongée en profondeur)
Historiquement, les expressions régulières viennent de la théorie formelle des langages. Bien avant C, elles étaient utilisées dans des éditeurs de texte comme `sed` et `awk`. En C, `<regex.h>` n'est pas la seule option ; il y a aussi des bibliothèques comme PCRE (Perl Compatible Regular Expressions). Elles peuvent offrir plus de fonctionnalités mais ne sont pas standard. Les implémentations standardisées comme POSIX peuvent varier en performance et en compatibilité selon les systèmes.

## See Also (Voir aussi)
- Documentation sur POSIX regex (expressions régulières) : https://man7.org/linux/man-pages/man7/regex.7.html
- PCRE - Perl Compatible Regular Expressions : https://www.pcre.org/
- Article Wiki sur les expressions régulières : https://fr.wikipedia.org/wiki/Expression_r%C3%A9guli%C3%A8re
