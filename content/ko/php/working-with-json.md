---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
JSON은 데이터 교환 포맷입니다. 프로그래머가 JSON을 사용하여 데이터를 간결하며 이해하기 쉽게 주고받습니다.

## How to: (방법)
PHP에서 JSON을 다루는 기본은 `json_encode()`와 `json_decode()` 함수입니다.

```PHP
<?php
// 배열을 JSON으로 변환
$array = ['name' => '홍길동', 'age' => 30, 'city' => '서울'];
$json = json_encode($array, JSON_UNESCAPED_UNICODE);
echo $json;
```

Sample Output:
```
{"name":"홍길동","age":30,"city":"서울"}
```

```PHP
<?php
// JSON을 PHP 배열로 변환
$json = '{"name":"홍길동","age":30,"city":"서울"}';
$array = json_decode($json, true);
print_r($array);
```

Sample Output:
```
Array
(
    [name] => 홍길동
    [age] => 30
    [city] => 서울
)
```

## Deep Dive (심층 분석)
JSON(JavaScript Object Notation)은 2001년에 등장했습니다. XML이나 YAML 같은 다른 데이터 포맷도 있지만, JSON이 가벼움과 읽기 쉬움 때문에 널리 쓰입니다. PHP에서 `json_encode()` 사용 시 옵션을 줄 수 있어 세밀한 제어가 가능합니다. 예를 들어 `JSON_PRETTY_PRINT`, `JSON_UNESCAPED_SLASHES` 등이 있습니다. PHP 7.3부터는 JSON 에러 처리가 향상되었습니다.

## See Also (관련 링크)
- PHP Manual on JSON: [https://www.php.net/manual/en/book.json.php](https://www.php.net/manual/en/book.json.php)
- JSON 공식 웹사이트: [https://www.json.org/json-ko.html](https://www.json.org/json-ko.html)
- PHP Manual on `json_encode`: [https://www.php.net/manual/en/function.json-encode.php](https://www.php.net/manual/en/function.json-encode.php)
- PHP Manual on `json_decode`: [https://www.php.net/manual/en/function.json-decode.php](https://www.php.net/manual/en/function.json-decode.php)
