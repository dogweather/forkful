---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att starta ett nytt projekt i programmeringsvärlden menar vi processen för att skapa grundstrukturen för en ny app eller tjänst. Programmerare gör detta för att tydligt strukturera deras kod, vilket underlättar utvecklingsprocessen och gör det lättare att hantera och underhålla koden.

## Hur man:
För att starta ett nytt Python-projekt behöver du en mapp att arbeta i. Använd `mkdir` för att skapa en ny mapp och `cd` för att gå in i den.
```Python
mkdir new_project
cd new_project
```

Skapa en ny Python-fil med `touch`.
```Python
touch main.py
```

Öppna `main.py` och skapa en enkel "Hello World"-applikation.
```Python
print("Hello World")
```

Kör din app.
```Python
python main.py
```

Du borde se "Hello World" skrivas ut på skärmen.

## Djupdykning
Att starta ett nytt projekt kan vara en komplex uppgift. Historiskt sett har det inneburit att noggrant planera ut kodstruktur, knåpa ihop ramverk och säkerställa att hela teamet är på samma sida. I vissa projekt kan detta innebära att använda specifika templates eller böjelser, såsom Django-skriptet `startproject` i Python-sammanhang.

Det finns dock alternativ till denna process, såsom att använda "Boilerplate code" eller färdiga projektmallar som kommer med en grundläggande kodstruktur. Dessa kan ge ditt projekt en snabbstart, men kanske inte tillåter samma grad av anpassning. Se till att du väljer rätt metod för projektet!

## Se även
1. Python's officiella dokumentation: https://docs.python.org/3/tutorial/modules.html
2. Django's `startproject`-dokumentation: https://docs.djangoproject.com/en/3.2/ref/django-admin/#startproject
3. En lista med Python Boilerplates: https://github.com/mattmakai/fullstackpython.com/blob/master/source/content/posts/160604-top-12-projects.markdown