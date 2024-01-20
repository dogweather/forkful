---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'impression de sortie de débogage est une méthode utilisée pour tracer le déroulement d'un programme pour repérer les erreurs. Les programmeurs le font pour comprendre comment le code fonctionne (ou ne fonctionne pas) et pour localiser les erreurs plus facilement.

## Comment faire :

Pour imprimer une sortie de débogage dans Ruby, utilisez la méthode `p`. Cette méthode imprime la valeur de l'expression fournie et retourne cette valeur.

```Ruby
debug_variable = "Test de débogage."
p debug_variable
#=> "Test de débogage."
```

## Regard approfondi

(1) Historiquement, l'impression de sortie de débogage été l'une des premières méthodes utilisées pour le débogage. Elle est toujours largement utilisée en raison de sa simplicité et de son efficacité. 

(2) Il existe d'autres méthodes pour le débogage en Ruby, tels que l'usage des débogueurs tels que "byebug" ou "pry".

(3) L'implémentation `p` dans Ruby est en fait une méthode de classe `Kernel`, donc disponible dans tous les objets Ruby. Elle est semblable à la méthod `puts`, mais `p` retourne également la valeur, ce qui est utile pour le débogage.

## Voir aussi

Pour plus d'informations sur le débogage dans Ruby :

1. http://ruby-doc.org/core-2.6.3/Kernel.html#method-i-p
2. https://guides.rubyonrails.org/debugging_rails_applications.html
3. http://ruby.bastardsbook.com/chapters/debugging/