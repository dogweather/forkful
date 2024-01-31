---
title:                "Création d'un fichier temporaire"
date:                  2024-01-20T17:40:24.686248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Créer un fichier temporaire, c'est like donner un coin d'éphémère à vos données. Les programmeurs font ça pour stocker des trucs vite fait ou tester des idées sans salir leur système de fichiers permanent.

## Comment faire :
Créons un fichier temporaire en Go :

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	tempFile, err := ioutil.TempFile("", "exemple")
	if err != nil {
		log.Fatal(err)
	}

	defer tempFile.Close()

	fmt.Printf("Fichier temporaire créé: %s\n", tempFile.Name())

	// Écrivons quelque chose dans le fichier
	_, err = tempFile.Write([]byte("Salut, ceci est un test!"))
	if err != nil {
		log.Fatal(err)
	}
}
```

Sortie probable :

```
Fichier temporaire créé: /tmp/exemple123456
```

## Approfondissement
Les fichiers temporaires existent depuis la nuit des temps - ou du moins, depuis les débuts de l'informatique moderne. C'est l'alternative silencieuse à la création de fichiers qui restent éternellement. En Go, `ioutil.TempFile` fait le gros du travail, mais en coulisses, il choisit un emplacement sur votre système (souvent `/tmp` sous Unix), crée un nom de fichier unique pour éviter les collisions, et ouvre le fichier pour vous.

Pourquoi ne pas juste créer un fichier normalement? Parce que la conscience est clé : la propreté et l'ordre aussi. Fichiers temp, ça signifie qu'ils ne sont pas là pour durer. Ils évitent le ménage après des tests ou des opérations one-off et se font oublier après redémarrage ou par un coup de `os.Remove` en Go.

Alternativement, dans Go, vous avez aussi `ioutil.TempDir` pour les dossiers temporaires, ou vous pourriez gérer manuellement vos fichiers avec `os` si vous êtes du genre contrôle absolu.

## Voir aussi
Explorez plus ?

- La doc officielle de Go pour `ioutil.TempFile`: https://golang.org/pkg/io/ioutil/#TempFile
- Tutoriel Go sur la gestion des fichiers : https://golang.org/doc/articles/wiki/
- Pour les puristes, la spec de Go : https://golang.org/ref/spec
