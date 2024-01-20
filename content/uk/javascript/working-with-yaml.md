---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?

YAML — це формат представлення даних, яким люблять користуватися для конфігурації через його чистоту та зручність. Програмісти використовують YAML, бо він легко читається людиною і гарно вписується в автоматизаційні сценарії.

## Як це зробити:

Для роботи з YAML у JavaScript, потрібно встановити пакет, наприклад `js-yaml`. Використовуйте команду `npm`:

```bash
npm install js-yaml
```

Код для парсингу YAML у JS виглядає так:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const config = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
  console.error(e);
}
```

Якщо у `config.yaml` є:

```yaml
version: 1
services:
  website:
    image: "nginx:alpine"
```

Виведе наступне:

```javascript
{ version: 1, services: { website: { image: 'nginx:alpine' } } }
```

## Поглиблений огляд:

YAML (YAML Ain't Markup Language) з'явився у 2001 році як альтернатива XML та іншим форматам. Головні переваги YAML - його простота і зрозумілість. У JavaScript, основна альтернатива YAML - це JSON, який також легко читається і підтримується без додаткових бібліотек. При роботі з YAML важливо стежити за вирівнюванням, оскільки відступи визначають структуру даних.

## Дивіться також:

- Документація `js-yaml`: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- YAML офіційний сайт: [https://yaml.org/](https://yaml.org/)
- JSON vs YAML аналіз: [https://www.json2yaml.com/](https://www.json2yaml.com/)