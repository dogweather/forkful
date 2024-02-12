---
title:                "Att påbörja ett nytt projekt"
aliases: - /sv/bash/starting-a-new-project.md
date:                  2024-01-20T18:02:58.961671-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att påbörja ett nytt projekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När du startar ett nytt projekt skapar du en grund för din kod. Programmerare gör detta för att organisera och strukturera sitt arbete från början.

## Kom igång:
Så här sätter du upp en grundläggande projektstruktur:

```Bash
mkdir MyNewProject
cd MyNewProject
mkdir {bin,src,doc,tests}
echo "#!/bin/bash" > bin/run-my-project.sh
chmod +x bin/run-my-project.sh
echo "My New Project" > README.md
git init .
```

Resultat:

```
MyNewProject/
├── bin
│   └── run-my-project.sh
├── src
├── doc
├── tests
└── README.md
```

## Fördjupning
Att starta ett nytt projekt har sina rötter i de tidiga dagarnas programmering, när struktur och ordning var nödvändig för att hantera stora kodbaser manuellt. Alternativ till detta inkluderar olika verktyg såsom Yeoman eller att använda program som `create-react-app` för specifika behov. Detaljer som man bör tänka på är t.ex. vilka kataloger som kommer behövas och hur du kan använda `git` för versionshantering från början.

## Se även
- [GitHub - How to start a new project](https://guides.github.com/activities/hello-world/)
- [GNU Bash documentation](https://www.gnu.org/software/bash/)
- [Atlassian - Set up a git repository](https://www.atlassian.com/git/tutorials/setting-up-a-repository)
