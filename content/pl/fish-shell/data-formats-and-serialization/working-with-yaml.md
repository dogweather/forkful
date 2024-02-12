---
title:                "Praca z YAML"
date:                  2024-02-03T19:25:28.389974-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z YAML polega na analizie i manipulacji plikami YAML (YAML Ain't Markup Language), formacie serializacji danych używanym dla plików konfiguracyjnych, w środowisku Fish Shell. Programiści robią to w celu automatyzacji i konfiguracji aplikacji lub usług w sposób efektywny w kontekście środowisk powłoki, ułatwiając zadania takie jak zarządzanie konfiguracjami i przydzielanie zasobów.

## Jak to zrobić:
Fish Shell nie ma wbudowanego wsparcia do analizowania plików YAML, ale możesz wykorzystać narzędzia stron trzecich takie jak `yq` (lekki i przenośny procesor YAML działający z linii komend), aby obsługiwać dane YAML.

**Instalacja yq (jeśli nie jest już zainstalowany):**
```fish
sudo apt-get install yq
```

**Odczytywanie wartości z pliku YAML:**
Załóżmy, że masz plik YAML `config.yaml` o następującej zawartości:
```yaml
database:
  host: localhost
  port: 3306
```

Aby odczytać host bazy danych, użyłbyś:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Przykładowe wyjście:**
```
localhost
```

**Aktualizacja wartości w pliku YAML:**
Aby zaktualizować `port` na `5432`, użyj:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Weryfikacja aktualizacji:**
```fish
yq e '.database.port' config.yaml
```
**Przykładowe wyjście:**
```
5432
```

**Tworzenie nowego pliku YAML:**
Aby stworzyć nowy `new_config.yaml` z zdefiniowaną zawartością:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
To używa `yq` do przetwarzania i ładnego wydruku (-P flag) ciągu znaków do nowego pliku YAML.

**Analiza skomplikowanych struktur:**
Jeśli masz bardziej skomplikowany plik YAML i potrzebujesz pobrać zagnieżdżone tablice lub obiekty, możesz:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Przykładowe wyjście:**
```
server1
server2
```
Używając `yq`, Fish Shell ułatwia nawigację po dokumentach YAML i manipulowanie nimi dla różnych zadań automatyzacji i konfiguracji.
