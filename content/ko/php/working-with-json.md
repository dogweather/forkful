---
title:                "JSON 작업하기"
html_title:           "PHP: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON은 JavaScript Object Notation의 약자로, 데이터를 저장하고 전송하기 위해 사용하는 형식입니다. 프로그래머들은 이것을 사용하여 다른 시스템과 데이터를 주고 받을 수 있습니다.

## 방법:

### JSON에서 데이터를 읽기

```PHP
<?php
// JSON 형식으로 변환된 데이터
$json_data = '{"name": "John", "age": "30"}';

// JSON 형식의 데이터를 PHP에서 읽기
$data = json_decode($json_data);

// 데이터 출력
echo $data->name; // John
echo $data->age; // 30
?>
```

### 데이터를 JSON 형식으로 변환하기

```PHP
<?php
// PHP 배열 생성
$student = array("name" => "Jane", "age" => "25");

// JSON 형식으로 변환
$json_data = json_encode($student);

// 변환된 데이터 출력
echo $json_data; // {"name": "Jane", "age": "25"}
?>
```

## 자세히 살펴보기:

### 역사적 맥락

JSON 형식은 2001년에 더글러스 크록퍼드(Douglas Crockford)에 의해 발명되었습니다. 그 이후로 많은 프로그래밍 언어에 채택되고 있습니다.

### 대안

JSON의 대안으로는 XML이 있습니다. 하지만 JSON은 보다 가볍고 간결하며, JavaScript에서 바로 사용할 수 있다는 점에서 XML보다 많은 인기를 얻고 있습니다.

### 구현 세부사항

PHP에서는 ```json_decode()``` 함수를 사용하여 JSON 형식의 데이터를 PHP 배열로 변환하고, ```json_encode()``` 함수를 사용하여 PHP 배열을 JSON 형식으로 변환할 수 있습니다.

## 관련 자료:

- [JSON in 5 Minutes](https://json.org/json-ko.html)