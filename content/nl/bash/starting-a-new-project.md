---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:20.857753-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw project starten betekent vaak het creëren van een mappenstructuur en initiële bestanden - een beetje zoals het leggen van de fundering van een huis. Programmeurs doen dit om gedachten, bestanden en taken te organiseren, chaos om te zetten in een nette to-do lijst.

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
