---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
YAML – це формат даних, який легко читається людиною, із широким використанням для конфігураційних файлів. Програмісти використовують YAML через його простоту та зручність у налаштуванні середовищ та застосунків.

## Як це робити:
```PHP
<?php
$symfonyYaml = new Symfony\Component\Yaml\Yaml();
$array = ['name' => 'Олег', 'occupation' => 'веб-розробник'];

// Конвертація масиву в YAML
$yaml = $symfonyYaml->dump($array);
echo $yaml;

// Конвертація YAML назад у масив
$parsed = $symfonyYaml->parse($yaml);
print_r($parsed);
```
Sample output:
```yaml
name: Олег
occupation: веб-розробник
```
```php
Array
(
    [name] => Олег
    [occupation] => веб-розробник
)
```

## Глибше занурення:
YAML (YAML Ain't Markup Language) з'явився на початку 2000-х як альтернатива XML та JSON для зберігання та обміну даними. У PHP можна працювати з YAML використовуючи розширення PECL YAML або бібліотеки на кшталт Symfony YAML компонента. Цей формат зручний для складних конфігурацій, адже легко піддається модифікаціям людиною і добре підтримує вкладені структури.

## Також дивіться:
- Офіційний сайт YAML: https://yaml.org
- Symfony YAML компонент документація: https://symfony.com/doc/current/components/yaml.html
- PECL YAML розширення: https://pecl.php.net/package/yaml