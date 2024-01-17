---
title:                "Yaml 작업하기"
html_title:           "PHP: Yaml 작업하기"
simple_title:         "Yaml 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML은 PHP과 함께 작동하는 데이터 직렬화 형식이다. 이는 데이터를 구조화하기 쉽고 보기 쉬운 방식으로 저장하고 전송하기 위해 사용된다. 프로그래머들은 YAML을 사용함으로써 코드의 가독성을 높이고 데이터를 관리하는 작업을 간단하게 만들 수 있다.

## How to:

### Create a YAML file:

```PHP
<?php

$data = array(
    'name' => 'John Doe',
    'age' => 25,
    'occupation' => 'Web Developer'
);

$file = fopen('data.yaml', 'w');
fwrite($file, yaml_emit($data));
fclose($file);
```

Sample output:

```yaml
age: 25
name: John Doe
occupation: Web Developer
```

### Read a YAML file:

```PHP
<?php

$file = file_get_contents('data.yaml');
$data = yaml_parse($file);

echo $data['name'] . ' is ' . $data['age'] . ' years old and works as a ' . $data['occupation'];
```

Sample output:

```
John Doe is 25 years old and works as a Web Developer
```

## Deep Dive

YAML은 2001년에 개발된 프로그래밍 언어인 Perl을 기반으로 만들어졌다. 이는 효율적인 방식으로 데이터를 구조화하고 표현하기 위해 만들어졌으며, 쉽고 적은 코드를 사용하여 데이터를 생성하고 읽는 것을 가능하게 한다. YAML은 다른 데이터 직렬화 형식인 JSON과 비슷한 구조를 가지고 있지만, 더 간단하고 직관적인 문법을 사용한다는 장점이 있다.

다른 대안으로는 XML이 있지만, YAML은 코드의 가독성을 높여주고 작업을 더 효율적으로 만들어 준다는 점에서 더 인기가 있다. 또한 PHP에서는 내장 라이브러리로서 YAML을 지원하므로 따로 추가적인 라이브러리를 사용할 필요가 없다.

YAML은 데이터를 구조화하는 편리한 방법이지만, 문자열에 특수 문자를 추가하는 것에 있어서는 주의해야 한다. 이는 코드의 문제를 일으킬 수 있으므로 해결하는 방법을 잘 알고 사용해야 한다.

## See Also

- [Official YAML website](http://yaml.org/)
- [PHP Manual on YAML functions](https://www.php.net/manual/en/ref.yaml.php)
- [Comparison of YAML and JSON](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats#JSON_vs._YAML)