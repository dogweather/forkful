---
title:                "Journalisation"
aliases:
- fr/fish-shell/logging.md
date:                  2024-01-26T01:02:48.266092-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Le journalisation, c'est essentiellement consigner ce que fait votre application - un journal intime, si vous voulez, mais pour le code. Les programmeurs le font pour garder une trace des détails techniques, comme les changements d'état, les événements systèmes et les bugs agaçants, en s'assurant qu'aucun accroc ne passe inaperçu.

## Comment faire :
Dans Fish, la journalisation peut être aussi simple que de rediriger les flux de sortie standard et d'erreur vers un fichier. Faisons une entrée de journal pour les heures de début et de fin de notre script.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script démarré" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script terminé" >> my_app.log
end

log_start
# ... les tâches de votre script ...
log_end

cat my_app.log
```

Voici ce que vous verriez dans `my_app.log` :

```
2023-04-01 10:35:47  - Script démarré
2023-04-01 10:36:02  - Script terminé
```

Pour une journalisation avancée, vous pouvez utiliser des fonctions avec des paramètres pour le niveau de journalisation et les messages :

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Ceci est un message informatif."
log_message ERROR "Quelque chose s'est mal passé !"
```

Exemple de sortie `my_app.log` :
```
2023-04-01 10:35:47 [INFO] Ceci est un message informatif.
2023-04-01 10:35:49 [ERROR] Quelque chose s'est mal passé !
```

## Exploration Approfondie
Historiquement, la journalisation dans les scripts shell se faisait avec une multitude de commandes `echo`, et bien que cela soit toujours une option, la mise en œuvre de systèmes plus complexes peut être un défi. Fish n'a pas un mécanisme de journalisation intégré comme certains autres shells ou langages de programmation, donc vous devez souvent concevoir le vôtre.

Les alternatives à la commande intégrée `echo` de Fish pour la journalisation incluent des outils Unix comme `syslog` ou `logger`, qui interagissent avec le daemon de journalisation du système, offrant une approche plus intégrée de la journalisation des événements système.

La simplicité de Fish vous permet de créer des fonctions pour gérer la verbosité de la journalisation, en définissant différents niveaux que vous pouvez activer ou désactiver. Certaines implémentations peuvent même inclure le nom du script, le numéro de ligne et l'horodatage, ce qui facilite la traçabilité des étapes qui ont conduit à un événement.

## Voir aussi
- La documentation de Fish Shell sur l'écriture de fonctions : https://fishshell.com/docs/current/#syntax-function
- Conseils de base pour l'écriture de scripts shell : https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guide du protocole Syslog : https://tools.ietf.org/html/rfc5424
