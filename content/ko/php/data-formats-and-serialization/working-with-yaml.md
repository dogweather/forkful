---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:27.210403-07:00
description: "\uBC29\uBC95: PHP\uB294 \uD604\uC7AC \uBC84\uC804\uC5D0\uC11C \uD45C\
  \uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uC77C\uBD80\uB85C YAML\uC744 \uD30C\
  \uC2F1\uD558\uB294 \uAC83\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4\
  . PHP\uC5D0\uC11C YAML\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C\
  \ \uBC29\uBC95\uC740 Symfony YAML \uCEF4\uD3EC\uB10C\uD2B8\uB098 `yaml` PECL \uD655\
  \uC7A5\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:57.081174-06:00'
model: gpt-4-0125-preview
summary: "PHP\uB294 \uD604\uC7AC \uBC84\uC804\uC5D0\uC11C \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC758 \uC77C\uBD80\uB85C YAML\uC744 \uD30C\uC2F1\uD558\uB294\
  \ \uAC83\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
PHP는 현재 버전에서 표준 라이브러리의 일부로 YAML을 파싱하는 것을 지원하지 않습니다. PHP에서 YAML을 사용하는 가장 간단한 방법은 Symfony YAML 컴포넌트나 `yaml` PECL 확장을 사용하는 것입니다.

### Symfony YAML 컴포넌트 사용하기
먼저, Composer를 통해 Symfony YAML 컴포넌트를 설치하세요:

```bash
composer require symfony/yaml
```

그런 다음, 다음과 같이 YAML 콘텐츠를 파싱하고 덤프할 수 있습니다:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML 파싱
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// 배열에서 YAML 생성하기
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

파싱 시 샘플 출력:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

덤프 시 샘플 출력:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### `yaml` PECL 확장 사용하기
원한다면, 또는 프로젝트 요구 사항이 허용한다면, PECL 확장은 YAML을 다루는 또 다른 효율적인 방법일 수 있습니다. 우선, 확장 프로그램이 설치되어 있는지 확인하세요:

```bash
pecl install yaml
```

그런 다음, `php.ini` 설정에서 활성화하세요:

```ini
extension=yaml.so
```

다음은 YAML을 파싱하고 내보내는 방법입니다:

```php
<?php

// YAML 파싱
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// 배열에서 YAML 생성하기
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

출력 결과는 Symfony 컴포넌트의 것과 비슷할 것이며, YAML이 사람이 읽을 수 있는 형식과 PHP 배열 구조 사이의 다리 역할을 함으로써, 구성 및 데이터 처리를 더 쉽게 만듦을 보여줍니다.
