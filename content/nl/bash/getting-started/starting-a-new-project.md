---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:20.857753-07:00
description: 'Hoe te: Laten we een eenvoudig script maken om een nieuw project op
  te starten.'
lastmod: '2024-03-13T22:44:50.983167-06:00'
model: gpt-4-0125-preview
summary: Laten we een eenvoudig script maken om een nieuw project op te starten.
title: Een nieuw project starten
weight: 1
---

## Hoe te:
Laten we een eenvoudig script maken om een nieuw project op te starten.

```Bash
#!/bin/bash

# Project setup script

PROJECT_NAME=$1
BASE_DIR=$(pwd)

# Functie om directories te maken
make_directories() {
    mkdir -p $PROJECT_NAME/{bin,src,doc,test}
    echo "Directories aangemaakt."
}

# Functie om initiële bestanden te maken
make_files() {
    touch $PROJECT_NAME/README.md
    touch $PROJECT_NAME/src/main.sh
    echo "#!/bin/bash" > $PROJECT_NAME/src/main.sh
    chmod +x $PROJECT_NAME/src/main.sh
    echo "Initiële bestanden aangemaakt."
}

# Functie om een git repository te initialiseren
init_git() {
    cd $PROJECT_NAME
    git init
    cd $BASE_DIR
    echo "Git repository geïnitialiseerd."
}

# Hoofd uitvoering
if [ -z "$PROJECT_NAME" ]; then
    echo "Geef a.u.b. een projectnaam op."
else
    make_directories
    make_files
    init_git
    echo "Project '$PROJECT_NAME' aangemaakt."
fi
```
Voorbeelduitvoer na het uitvoeren van `bash setup.sh myproject`:

```Bash
Directories aangemaakt.
Initiële bestanden aangemaakt.
Leeg Git repository geïnitialiseerd in /pad/naar/myproject/.git/
Project 'myproject' aangemaakt.
```

## Diepere Duik
Voordat we scripts hadden, maakten we handmatig directories en bestanden elke keer aan - vermoeiend en foutgevoelig. Automatisering met een script minimaliseert fouten en versnelt dingen.

Alternatieven zijn onder andere tools zoals Yeoman, die projecten in verschillende talen opzet, maar dat is als het gebruiken van een boormachine wanneer je een punaise nodig hebt.

Het script hierboven is bewust eenvoudig. Het maakt een projectdirectory, subdirectories voor organisatie (zoals `src` voor broncode), en essentiële bestanden (zoals `README.md`). Daarnaast zet het een Git-repo op zodat je versies van je werk kunt opslaan. Je kunt het aanpassen en toevoegen aan de behoeften van elk project.

## Zie Ook
- Git documentatie: https://git-scm.com/doc
- Yeoman: http://yeoman.io/
- Bash scripting tutorials: https://www.shellscript.sh/
