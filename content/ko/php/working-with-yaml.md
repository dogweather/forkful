---
title:                "yaml로 작업하기"
html_title:           "PHP: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML과 함께 작업할까요?

먼저, YAML은 PHP에서 설정 파일과 데이터를 쉽게 다룰 수 있는 형식입니다. 또한 YAML은 다른 형식보다 가독성이 좋고 사용하기도 간단하기 때문에 개발자들 사이에서 인기가 높습니다.

## 어떻게 YAML을 사용할까요?

YAML을 PHP에서 사용하는 방법은 매우 간단합니다. 먼저, YAML 파일을 불러오기 위해 `yaml_parse_file()` 함수를 사용합니다.

```PHP
// YAML 파일 불러오기
$data = yaml_parse_file('config.yml');

// 배열 구조로 변환
var_dump($data);
```

출력 결과는 다음과 같습니다.

```PHP
array(3) {
  ["name"]=>
  string(6) "John"
  ["age"]=>
  int(25)
  ["favorite_color"]=>
  string(5) "blue"
}
```

YAML 파일 내용은 PHP 배열 구조로 변환되어 반환됩니다. 이제 이 데이터를 `foreach` 루프를 이용해 출력할 수 있습니다.

```PHP
// 루프를 이용한 출력
foreach ($data as $key => $value) {
  echo "$key: $value \n";
}
```

위 코드의 출력 결과는 다음과 같습니다.

```
name: John
age: 25
favorite_color: blue
```

## 깊이 들어가기

YAML은 다양한 데이터 타입을 지원하며, 배열과 마찬가지로 하위 요소로 다른 데이터 타입을 포함할 수 있습니다. 하지만 배열과 달리 키-값 쌍으로 구성되기 때문에 데이터를 보다 가독성 좋게 표현할 수 있습니다.

예를 들어, 다음과 같은 YAML 파일이 있다고 가정해봅시다.

```yaml
name: John Smith
age: 35
address:
  city: New York
  state: NY
  country: USA
```

이를 PHP에서 다루기 위해서는 `address`라는 키를 가지는 배열을 추가하고 해당 배열 내부에 `city`, `state`, `country`와 같은 키를 가진 요소를 추가해주면 됩니다. 이를 코드로 표현하면 다음과 같습니다.

```PHP
// YAML 파일 불러오기
$data = yaml_parse_file('user.yml');

// 배열 구조로 변환
var_dump($data);
```

출력 결과는 다음과 같습니다.

```PHP
array(3) {
  ["name"]=>
  string(11) "John Smith"
  ["age"]=>
  int(35)
  ["address"]=>
  array(3) {
    ["city"]=>
    string(8) "New York"
    ["state"]=>
    string(2) "NY"
    ["country"]=>
    string(3) "USA"
  }
}
```

이제 `foreach` 루프를 이용해 데이터를 출력해보겠습니다.

```PHP
// 루프를 이용한 출력
foreach ($data as $key => $value) {
  if ($key == 'address') {
    echo "Address: \n";
    foreach ($value as $addressKey => $addressValue) {
      echo "  $addressKey: $addressValue \n";
    }
    echo "\n";
  } else {
    echo "$key: $value \n";
  }
}
```

결과는 다음과 같습니다.

```
name: John Smith 
age: 35 
Address: 
  city: New York 
  state: NY 
  country: USA
```

## 더 알아보기

현재 PHP 버전에서는 `yaml_parse()` 함수도 제공되지만, 이 함수는 PHP 7.2 이후로는 deprecated되었습니다. 따라서 새로 작업하시는 경우에는 `yaml_parse_file()`를 사용하는 것을 권장합니다.

또한 YAML을 다루는 라이브러리인 "Symfony YAML"이 있으니 참고하