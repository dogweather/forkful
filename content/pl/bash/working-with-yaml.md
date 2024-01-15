---
title:                "Praca z yaml"
html_title:           "Bash: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą i często spotykasz się z koniecznością przetwarzania plików konfiguracyjnych lub danych, to jest szansa, że napotkałeś format YAML. Pozostawienie ją wygodnym formatem z formatowaniem, który można odczytywać przez ludzi i maszyny. W ten sposób Jest to przydatne narzędzie do automatyzacji procesów, a także do przechowywania i przesyłania danych w sposób, który jest zrozumiały dla każdego.

## Jak to zrobić

### Tworzenie pliku YAML
```Bash
touch plik.yaml
```
W powyższym przykładzie tworzymy nowy plik YAML przy użyciu polecenia "touch". Można również używać innego edytora tekstowego, na przykład Sublime lub Atom, aby utworzyć lub edytować plik YAML.

### Wpisywanie danych w pliku YAML
```Bash
open plik.yaml
```
Użyj wybranego edytora tekstowego, aby otworzyć plik YAML i wpisać dane. Pamiętaj, że format YAML jest zależny od wcięć, więc ważne jest, aby zachować odpowiednią strukturę dokładając spację lub tabulację.

### Przetwarzanie pliku YAML
```Bash
cat plik.yaml
```
Polecenie "cat" wyświetla zawartość pliku YAML. Można również przekazać dane z pliku do innych komend za pomocą potoku "|" lub użyć ich jako argumentów dla innych poleceń.

## Wgląd w wykorzystywanie YAML
YAML jest formatem danych zorientowanym na człowieka, co oznacza, że jest łatwiejszy w czytaniu niż formularz JSON. Można również umieszczać komentarze w plikach YAML, co czyni je jeszcze wygodniejszymi w użyciu. Wielu programistów wykorzystuje format YAML do przechowywania konfiguracji swoich aplikacji lub jako sposób na przechowywanie danych w plikach tekstowych.

## Zobacz także
- https://yaml.org
- https://www.linux.com/training-tutorials/beginners-guide-yaml/
- https://www.tutorialspoint.com/yaml/index.htm