---
title:                "Travailler avec TOML"
aliases:
- fr/php/working-with-toml.md
date:                  2024-01-26T04:24:32.114954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
TOML, abréviation de Tom's Obvious, Minimal Language, est un format de données semblable à JSON ou YAML, mais plus facile à lire pour les humains. Les programmeurs l'utilisent pour des fichiers de configuration car il est simple et se traduit bien en structures de données.

## Comment faire :
D'abord, assurez-vous d'avoir installé une bibliothèque d'analyse TOML, comme `yosymfony/toml`. Analysons un fichier TOML :

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[base_de_donnees]
serveur = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connexion_max = 5000
active = true
TOML;

$tableau = Toml::Parse($tomlString);

print_r($tableau);
```

Exemple de sortie :

```
Array
(
    [base_de_donnees] => Array
        (
            [serveur] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connexion_max] => 5000
            [active] => 1
        )

)
```
## Plongée en profondeur
TOML a été créé en 2013, conçu par le co-fondateur de GitHub, Tom Preston-Werner, comme une alternative plus conviviale aux fichiers de configuration XML et JSON. Tandis que JSON est simple pour les machines, la structure de TOML facilite la lecture pour les humains, sans la complexité de YAML.

Les alternatives à TOML incluent JSON, YAML et XML. Chacun possède ses forces et scénarios d'application. JSON est omniprésent et indépendant de la langue ; YAML est plus lisible et supporte les commentaires, tandis que XML est étendu et largement soutenu.

Lors de l'implémentation de TOML en PHP, vous regardez les bibliothèques qui analysent son contenu en tableaux ou objets PHP. `yosymfony/toml` est un analyseur PHP qui adhère à la spécification v0.4.0 de TOML. Pour rester à jour, vérifiez toujours les nouveaux analyseurs ou les mises à jour qui prennent en charge la version la plus récente de TOML (v1.0.0 à ma dernière mise à jour).

## Voir aussi
- Spécification TOML : <https://toml.io/>
- Analyseur TOML pour PHP (`yosymfony/toml`) : <https://github.com/yosymfony/toml>
- Comparaison des formats de données (XML, JSON, YAML, TOML) : <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Gestionnaire de paquets PHP (Composer) : <https://getcomposer.org/>
