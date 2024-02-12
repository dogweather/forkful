---
title:                "Робота з YAML"
aliases:
- /uk/arduino/working-with-yaml.md
date:                  2024-02-03T19:25:07.625892-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

YAML (YAML Ain't Markup Language) - це стандарт серіалізації даних, який легко читається людьми, і може використовуватися для файлів конфігурації, міжпрограмного спілкування та зберігання даних. Програмісти звертаються до YAML для проектів на Arduino, щоб спростити процес налаштування своїх застосунків, роблячи зміну параметрів простішою без поглиблення у код, підвищуючи зрозумілість та спрощуючи обмін конфігураціями.

## Як це робити:

Робота з YAML безпосередньо на Arduino не така проста, як у вищих програмних середовищах через обмеження пам'яті та відсутність вбудованих бібліотек обробки YAML. Однак, для проектів, яким потрібен аналіз або генерація YAML, типовий підхід включає використання супутнього комп'ютера (наприклад, Raspberry Pi) або конвертацію файлів YAML до більш пристосованого для Arduino формату (наприклад, JSON) за допомогою зовнішніх скриптів. Для демонстрації зосередимося на останньому підході за допомогою популярної бібліотеки: ArduinoJson.

**Крок 1:** Конвертуйте вашу конфігурацію YAML у JSON. Для цього можна використати онлайн-інструменти або утиліти командного рядка, такі як `yq`.

YAML файл (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Конвертовано у JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Крок 2:** Використовуйте бібліотеку ArduinoJson для аналізу JSON файлу у вашому скетчі для Arduino. Спершу, вам потрібно встановити бібліотеку ArduinoJson через Менеджер Бібліотек у Arduino IDE.

**Крок 3:** Завантажте та аналізуйте JSON у вашому коді. З огляду на обмеження пам'яті Arduino, уявіть, що рядок JSON зберігається у змінній або читається з SD-карти.

Зразок скетча для Arduino:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // Тут нічого немає у цьому прикладі
}
```

Результат після запуску скетча:
```
SSID: YourSSID
Password: YourPassword
```

Цей підхід, що включає конвертацію в JSON та використання бібліотеки ArduinoJson, дозволяє контролювати налаштування YAML у проектах Arduino, обходячи прямий аналіз YAML на мікроконтролері.
