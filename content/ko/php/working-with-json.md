---
title:                "Json으로 작업하기"
html_title:           "PHP: Json으로 작업하기"
simple_title:         "Json으로 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현대의 웹 개발에서 필수적인 데이터 형식입니다. PHP에서는 이러한 JSON 형식을 간편하게 다룰 수 있기 때문에, 웹 개발자라면 JSON에 대한 기본적인 이해가 필요합니다.

## 방법

```PHP
// 배열 생성
$fruits = array("apple", "banana", "orange");

// JSON 형식으로 인코딩
$json = json_encode($fruits);

// 인코딩된 JSON 출력
echo $json;

// 출력 결과: ["apple", "banana", "orange"]
```

위의 예제에서는 PHP의 `json_encode()` 함수를 사용하여 배열을 JSON 형식으로 변환한 후 출력하는 방법을 보여줍니다. 이와 같이 간단하게 PHP에서 JSON 형식을 다룰 수 있습니다.

### 연관 배열 다루기

```PHP
// 연관 배열 생성
$student = array(
  "name" => "John",
  "age" => 25,
  "major" => "Computer Science"
);

// JSON 형식으로 인코딩
$json = json_encode($student);

// 인코딩된 JSON 출력
echo $json;

// 출력 결과: {"name":"John","age":25,"major":"Computer Science"}
```

연관 배열을 사용하는 경우에도 `json_encode()` 함수를 이용하여 JSON 형식으로 변환할 수 있습니다. 이때 연관 배열의 키는 JSON 객체의 속성(property)으로 변환됩니다.

## 깊게 살펴보기

### JSON 데이터 가져오기

PHP에서는 `json_decode()` 함수를 사용하여 JSON 데이터를 읽어올 수 있습니다. 이 함수는 JSON 문자열을 PHP 배열로 변환해줍니다.

```PHP
// JSON 데이터 가져오기
$json_data = file_get_contents('data.json');

// JSON 데이터를 PHP 배열로 변환
$data = json_decode($json_data, true);
```

또는 위의 예제와 같이 `json_decode()` 함수에 두 번째 인자를 `true`로 전달하면, 연관 배열로 변환됩니다.

### JSON 데이터 유효성 검사

PHP에서는 `json_last_error()` 함수를 사용하여 JSON 데이터의 유효성을 검사할 수 있습니다. 이 함수는 JSON 데이터를 파싱할 때 발생한 마지막 에러를 반환합니다.

```PHP
// JSON 데이터 가져오기
$json_data = file_get_contents('data.json');

// JSON 데이터를 PHP 배열로 변환
$data = json_decode($json_data, true);

// 유효성 검사
if (json_last_error() === JSON_ERROR_NONE) {
  echo "JSON 데이터가 유효합니다.";
} else {
  echo "유효하지 않은 JSON 데이터입니다.";
}
```

위의 예제에서는 `json_last_error()` 함수를 사용하여 JSON 데이터가 유효한지를 검사하고 있습니다. 유효하지 않은 데이터일 경우, 해당 에러 코드를 확인할 수도 있습니다.

## 더 알아보기

JSON 형식을 다루는 데에는 더 많은 함수들이 있습니다. 자세한 내용은 PHP 공식 문서에서 확인할 수 있습니다.

[PHP - JSON 함수](https://www.php.net/manual/en/ref.json.php)

## 관련 링크

[언제 JSON을 사용하는 것이 좋을까?](https://medium.com/@StanAlters/when-to-use-json-5a1bde4ce51d)

[JSON에 대해 알아야 할 5가지 중요한 개념](https://www.c-sharpcorner.com/article/5-important-concepts-of-json-you-need-to-know/)