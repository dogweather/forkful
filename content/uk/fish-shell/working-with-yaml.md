---
title:                "Робота з YAML"
aliases:
- uk/fish-shell/working-with-yaml.md
date:                  2024-02-03T19:25:53.826262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Робота з YAML передбачає розбір і маніпуляції з файлами YAML (YAML Ain't Markup Language), формат серіалізації даних, що використовується для файлів конфігурації, в оболонці Fish Shell. Програмісти роблять це для автоматизації та налаштування додатків або служб ефективно у контексті оболонкових середовищ, сприяючи завданням як-от управління конфігураціями та розгортання ресурсів.

## Як це робити:
Fish Shell не має вбудованої підтримки для розбору YAML, але ви можете використовувати сторонні інструменти, як-от `yq` (легковаговий та портативний командний процесор YAML), для роботи з даними YAML.

**Встановлення yq (якщо ще не встановлено):**
```fish
sudo apt-get install yq
```

**Читання значення з файлу YAML:**
Припустимо, ви маєте файл YAML `config.yaml` з наступним вмістом:
```yaml
database:
  host: localhost
  port: 3306
```

Щоб прочитати хост бази даних, ви б використали:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Приклад виводу:**
```
localhost
```

**Оновлення значення у файлі YAML:**
Щоб оновити `port` до `5432`, використайте:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Перевірка оновлення:**
```fish
yq e '.database.port' config.yaml
```
**Приклад виводу:**
```
5432
```

**Створення нового файлу YAML:**
Для створення нового `new_config.yaml` із попередньо визначеним вмістом:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Це використовує `yq` для обробки та красивого виводу (-P прапор) рядка у новий файл YAML.

**Розбір складних структур:**
Якщо у вас є більш складний файл YAML і потрібно отримати вкладені масиви або об'єкти, ви можете:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Приклад виводу:**
```
server1
server2
```
Використовуючи `yq`, Fish Shell робить навігацію через документи YAML та їх маніпуляцію простими для різноманітних задач автоматизації та конфігурації.
