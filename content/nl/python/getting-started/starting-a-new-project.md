---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:22.143147-07:00
description: "Een nieuw project starten gaat helemaal over het cre\xEBren van een\
  \ verse map met bestanden opgezet voor je nieuwe code avontuur. Het is alsof je\
  \ de grond\u2026"
lastmod: 2024-02-19 22:05:09.464905
model: gpt-4-0125-preview
summary: "Een nieuw project starten gaat helemaal over het cre\xEBren van een verse\
  \ map met bestanden opgezet voor je nieuwe code avontuur. Het is alsof je de grond\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten gaat helemaal over het creëren van een verse map met bestanden opgezet voor je nieuwe code avontuur. Het is alsof je de grond breekt op een bouwterrein, maar dan voor programmeurs. We doen dit om ideeën om te zetten in werkende software, onze code te organiseren en vanaf het begin de complexiteit te beheren.

## Hoe:

Laten we een Python project starten. Eerst, maak een nieuwe map:

```bash
mkdir my_new_project
cd my_new_project
```

Nu, stel een virtuele omgeving in - dit houdt de afhankelijkheden van ons project netjes:

```bash
python -m venv venv
source venv/bin/activate # Op Windows, gebruik `venv\Scripts\activate`
```

Met ons virtuele land voorbereid, zaai de zaden van je project met een `main.py` bestand:

```bash
touch main.py
echo "print('Hallo, nieuw project!')" > main.py
python main.py
```

Uitvoer:
```plaintext
Hallo, nieuw project!
```

Voor de goede orde, laten we de afhankelijkheden vroeg vastleggen. Zelfs als deze nog niet bestaan:

```bash
pip freeze > requirements.txt
```

En dat is het embryo van je project. Vanaf hier groeit het.

## Diep Duiken

In het verleden zou menig programmeur het gewoon op de bonnefooi doen, beginnend met code in een eenzaam bestand. Chaos zou vaak volgen naarmate het project groeide. Tegenwoordig, hebben we betere praktijken.

Voor Python, hebben we conventies zoals PEP 8 voor stijlrichtlijnen. Er zijn ook tools zoals `cookiecutter` die projecten creëren vanuit sjablonen. Wil je een webapp? Er is een sjabloon voor. Het is opgezet om je tijd te besparen.

Aan de andere kant, vind je het misschien leuk om het handmatig te doen, zoals we hierboven hebben laten zien. Deze methode geeft je totale controle, je project opbouwend vanaf nul. Onthoud gewoon om de afhankelijkheden bij te houden met `requirements.txt`. Het is cruciaal voor wanneer je jouw project deelt of uitrolt.

## Zie Ook

- [De Liftgids voor Python](https://docs.python-guide.org/) - Een eigenzinnige gids over beste praktijken in Python.
- [PEP 8 -- Stijlgids voor Python Code](https://peps.python.org/pep-0008/) - De stijlbijbel voor Python ontwikkelaars.
- [Cookiecutter](https://github.com/cookiecutter/cookiecutter) - Een command-line hulpprogramma om projecten te creëren vanuit sjablonen.
