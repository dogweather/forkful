---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML은 데이터를 표현하기 위한 형식입니다. 코드에서 설정, 환경 변수를 다루거나 데이터를 저장하고 읽기 위해 YAML을 사용합니다. 간결하고 읽기 쉬워서 인기가 있죠.

## How to:
PHP에서 YAML을 다루려면 `yaml` 확장 기능을 설치해야 합니다. `yaml_parse`로 YAML을 PHP 배열로, `yaml_emit`으로 배열을 YAML로 바꿀 수 있어요.

```PHP
<?php
// YAML 문자열 파싱
$yamlString = "
user: john_doe
age: 35
children:
  - name: Jane
    age: 12
  - name: Bob
    age: 8
";

$array = yaml_parse($yamlString);
print_r($array);

// PHP 배열을 YAML 문자열로 변환
$arrayToYaml = [
  'user' => 'john_doe',
  'age' => 40,
  'children' => [
    ['name' => 'Jane', 'age' => 15],
    ['name' => 'Bob', 'age' => 11],
  ],
];

$yaml = yaml_emit($arrayToYaml);
echo $yaml;
?>
```

Sample output:
```
Array
(
    [user] => john_doe
    [age] => 35
    [children] => Array
        (
            [0] => Array
                (
                    [name] => Jane
                    [age] => 12
                )
            [1] => Array
                (
                    [name] => Bob
                    [age] => 8
                )
        )
)
...
```

## Deep Dive:
YAML은 "YAML Ain't Markup Language"의 재귀 약어로 2001년 개발되었습니다. JSON이나 XML 같은 다른 데이터 형식과 비교됩니다. YAML은 구조가 명확하고, 덜 복잡해서 설정 파일에 적합합니다. PHP에서 `symfony/yaml` 라이브러리를 통해 YAML을 다룰 수도 있습니다.

## See Also:
- Official YAML website: [https://yaml.org](https://yaml.org)
- PHP.net YAML functions: [https://www.php.net/manual/en/book.yaml.php](https://www.php.net/manual/en/book.yaml.php)
- Symfony YAML component: [https://symfony.com/doc/current/components/yaml.html](https://symfony.com/doc/current/components/yaml.html)
