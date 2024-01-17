---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Qu'est-ce que la recherche et le remplacement de texte?
La recherche et le remplacement de texte est une pratique courante des programmeurs pour modifier rapidement et efficacement des chaînes de caractères dans leur code. Cela permet de faire des changements massifs et répétitifs sans avoir à les faire un par un.

Pourquoi les programmeurs le font-ils?
Les programmeurs utilisent la recherche et le remplacement de texte pour gagner du temps et éviter les erreurs humaines. Au lieu de modifier manuellement chaque occurrence de texte, ils peuvent simplement utiliser une commande pour le faire automatiquement.

Comment faire:
Voici un exemple de code Arduino utilisant la fonction de recherche et de remplacement de texte pour remplacer toutes les occurrences de "Hello" par "Bonjour" dans une chaîne de caractères:

```
Arduino String message = "Hello World!";
message.replace("Hello", "Bonjour");
Serial.println(message);
```

Cela affichera "Bonjour World!" dans le moniteur série d'Arduino, montrant que la fonction de recherche et de remplacement a bien fonctionné.

Plongée en profondeur:
La recherche et le remplacement de texte a été introduite pour la première fois en 1964 par le programmeur Doug McIlroy. De nos jours, il existe de nombreuses alternatives telles que les expressions régulières, qui permettent une plus grande flexibilité dans la recherche et le remplacement de texte.

Voir aussi:
- Plus d'informations sur la fonction de recherche et de remplacement de texte dans la documentation officielle d'Arduino: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- Un tutoriel sur l'utilisation des expressions régulières pour la recherche et le remplacement de texte dans Arduino: [https://www.arduino.cc/reference/en/language/functions/communication/serial/print/](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)