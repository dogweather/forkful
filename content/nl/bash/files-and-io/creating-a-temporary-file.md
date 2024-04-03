---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:15.805125-07:00
description: 'Hoe: Bash heeft een ingebouwd commando genaamd `mktemp` om gemakkelijk
  tijdelijke bestanden te maken.'
lastmod: '2024-03-13T22:44:51.003113-06:00'
model: gpt-4-0125-preview
summary: Bash heeft een ingebouwd commando genaamd `mktemp` om gemakkelijk tijdelijke
  bestanden te maken.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
Bash heeft een ingebouwd commando genaamd `mktemp` om gemakkelijk tijdelijke bestanden te maken:

```Bash
# Een tijdelijk bestand aanmaken
temp_file=$(mktemp)

# Bekijk ons vers aangemaakte tijdelijke bestand
echo "Tijdelijk bestand aangemaakt: $temp_file"

# Gebruik het tijdelijke bestand
echo "Enige data" > "$temp_file"

# Lees het terug
cat "$temp_file"

# Opruimen: verwijder het bestand wanneer je klaar bent
rm "$temp_file"
```
Output:
```
Tijdelijk bestand aangemaakt: /tmp/tmp.Iy5nv69sed
Enige data
```

## Diepere Duik
Tijdelijke bestanden bestaan al in UNIX sinds de eerste dagen, waardoor gebruikers kunnen omgaan met tussenliggende gegevens zonder handmatige opruiming. In Bash scripting is `mktemp` de moderne aanpak, met opties voor het aanmaken van zowel bestanden (`mktemp`) als mappen (`mktemp -d`). Het commando maakt elke keer dat het wordt aangeroepen een uniek bestand, wat bestandscollisieproblemen vermijdt die zich voordoen wanneer meerdere instanties van een script of verschillende scripts tegelijkertijd worden uitgevoerd.

Voor `mktemp` zouden programmeurs handmatig bestanden aanmaken met namen waarvan zij hoopten dat ze uniek zouden zijn. Botsingen waren gebruikelijk, wat leidde tot gegevensverlies en beveiligingsproblemen. `mktemp` helpt dit te voorkomen door ervoor te zorgen dat de bestandsnaam uniek is met een mix van voorspelbare patronen en willekeurige tekens. In tegenstelling tot reguliere bestanden zijn deze tijdelijke bestanden bedoeld om na gebruik te worden verwijderd, waardoor het systeem netjes blijft.

Enkele alternatieven voor `mktemp` zijn het gebruik van `/dev/shm` voor tijdelijke bestanden in het geheugen, of er zelf een maken met datum en proces-ID (`$$`), maar deze methoden brengen meer risico's op conflicten met zich mee.

## Zie Ook
- De manpagina voor mktemp: voer `man mktemp` uit in Bash.
- [GNU Coreutils Handleiding](https://www.gnu.org/software/coreutils/manual/coreutils.html): voor details over standaard GNU/Linux-commando's.
- [Geavanceerde Bash-scriptinggids](https://www.tldp.org/LDP/abs/html/): voor meer complexe scriptingtechnieken en voorbeelden.
