---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
YAML — це формат серіалізації даних, легкий для людського сприйняття. Програмісти використовують YAML, щоб з легкістю записувати конфігурації, обмінюватись даними між мовами програмування та управляти інфраструктурою.

## Як це робити:
В Ruby працювати з YAML просто. Потрібна бібліотека `yaml`. Ось як ви можете серіалізувати і десеріалізувати YAML:

```Ruby
require 'yaml'

# Серіалізація Ruby об'єкта в YAML строку
my_data = { name: 'Ivan', occupation: 'Developer', location: 'Kyiv' }
yaml_string = my_data.to_yaml
puts yaml_string

# Десеріалізація YAML строки назад в Ruby об'єкт
parsed_data = YAML.load(yaml_string)
puts parsed_data
```

Це дасе вам наступний вивід:
```
---
:name: Ivan
:occupation: Developer
:location: Kyiv

{:name=>"Ivan", :occupation=>"Developer", :location=>"Kyiv"}
```

## Поглиблений розбір:
YAML (вимовляється як "ям-л" або "я-м-л") означає "YAML Ain't Markup Language" (YAML не є мовою розмітки). Попри це, спочатку це значило "Yet Another Markup Language". YAML виник на початку 2000-х. Він простіший і зручніший для людей, ніж XML або JSON для невеликих файлів. Альтернативи YAML — це JSON та TOML. Проте, YAML часто використовують у таких інструментах як Ansible, Docker та Kubernetes через його читабельність.

## Дивіться також:
- Офіційний сайт YAML: [https://yaml.org](https://yaml.org)
- Ruby YAML модуль документація: [https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html)
- YAML на GitHub, щоб побачити його специфікації: [https://github.com/yaml/yaml](https://github.com/yaml/yaml)
- YAML в Ruby on Rails: [https://guides.rubyonrails.org/configuring.html#using-the-config-for-yaml-configuration-files](https://guides.rubyonrails.org/configuring.html#using-the-config-for-yaml-configuration-files)