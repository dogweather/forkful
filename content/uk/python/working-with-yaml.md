---
title:                "Робота з yaml"
html_title:           "Python: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# Почему

Використання YAML є важливою частиною розробки програмного забезпечення, оскільки він надає зручний формат для збереження і обміну даними, що дозволяє легко взаємодіяти між різними програмами.

# Як

```Python 
import yaml 

# Створення словника з данними 
data = {'Ключ': [1,2,3], 
        'Ім\'я': 'Боб', 
        'Хобі': ['гра на гітарі', 'фотографування', 'подорожі'] 
} 
  
# Збереження даних у YAML форматі 
with open("дані.yaml", 'w') as file: 
    yaml.dump(data, file) 
  
# Завантаження даних з YAML файлу 
with open('дані.yaml') as file: 
    loaded_data = yaml.full_load(file) 
  
# Виведення значень із завантаженого словника 
print(loaded_data['ім\'я']) 
print(loaded_data['хобі']) 
```

## Глибоке вивчення

YAML - це формат даних, який базується на використанні індентування для представлення об'єктів та їх відношень у структурі ключ-значення. Такий підхід робить код читабельнішим і полегшує взаємодію з даними. Крім того, YAML підтримує вкладені об'єкти і списки, що дозволяє створювати складні структури даних.

## Див. також

- [Документація YAML](https://yaml.org/)
- [Документація бібліотеки PyYAML](https://pyyaml.org/)
- [Туторіал по роботі з YAML в Python](https://realpython.com/python-yaml/)