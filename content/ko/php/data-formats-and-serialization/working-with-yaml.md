---
title:                "YAML로 작업하기"
aliases:
- /ko/php/working-with-yaml.md
date:                  2024-02-03T19:26:27.210403-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 "YAML Ain't Markup Language"의 약자로, 사람이 읽을 수 있는 데이터 직렬화 형식입니다. 이는 일반적으로 구성 파일에 사용되며, 프로그래머들은 YAML을 그 단순성과 가독성 때문에 선호합니다. 이는 설정, 파라미터, 심지어 복잡한 데이터 구조를 쉽게 관리할 수 있는 형태로 저장하는 데에 탁월한 선택입니다.

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
