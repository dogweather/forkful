---
title:                "PHP: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

텍스트를 검색하고 교체하는 작업을 진행하는 이유는 무엇인가요? 이 게시물에서는 이 질문에 대한 간단한 답변을 드리겠습니다.

먼저, 텍스트를 검색하고 교체하는 것은 프로그래밍에서 매우 일반적인 작업입니다. 예를 들어, 사용자로부터 입력을 받은 뒤 입력된 값을 특정한 문자열로 대체하기 위해서는 검색과 교체가 필수적입니다. 또는 데이터베이스에서 사용자의 이름을 검색하고, 그 이름을 원하는 값으로 바꾸는 작업에도 검색과 교체가 사용됩니다. 따라서 텍스트를 검색하고 교체하는 것은 프로그래밍에서 필수적인 작업이라고 할 수 있습니다.

# 사용 방법

이제는 실제로 PHP를 사용하여 텍스트를 검색하고 교체하는 방법에 대해 알아보겠습니다. 아래의 예시 코드를 참고하여 실제로 텍스트를 검색하고 교체하는 프로그램을 작성해보세요.

```php
// 검색할 문자열
$search = "Hello";

// 교체할 문자열
$replace = "안녕하세요";

// 입력된 텍스트
$text = "Hello, world!";

// 검색과 교체 수행
$result = str_replace($search, $replace, $text);

// 결과 출력
echo $result; // 출력 결과: 안녕하세요, world!
```

위의 코드에서는 `str_replace()` 함수를 사용하여 텍스트를 검색하고 교체하였습니다. 이 함수는 검색할 문자열과 교체할 문자열, 그리고 입력된 텍스트를 인자로 받습니다. 그리고 해당 문자열을 검색하여 교체한 결과를 반환합니다.

또 다른 예시를 살펴보면, 배열을 사용하여 여러개의 문자열을 한 번에 검색 및 교체할 수도 있습니다.

```php
// 검색할 문자열들
$search = array("apple", "banana");

// 교체할 문자열들
$replace = array("사과", "바나나");

// 입력된 텍스트
$text = "I love apple and banana!";

// 검색과 교체 수행
$result = str_replace($search, $replace, $text);

// 결과 출력
echo $result; // 출력 결과: I love 사과 and 바나나!
```

이처럼 `str_replace()` 함수를 이용하여 텍스트를 검색하고 교체하는 작업을 쉽게 할 수 있습니다.

# 깊이 파헤치기

여기까지는 검색하고 교체하는 것의 기본적인 개념과 예시를 알아보았습니다. 하지만 실제로 PHP에서 텍스트를 다룰 때에는 많은 다양한 함수와 기법을 사용할 수 있습니다. 예를 들어, 검색 시 더 자세한 조건을 지정하고 싶을 경우 `str_replace()` 함수가 아닌 `preg_replace()` 함수를 사용하여 정규식을 이용할 수도 있습니다. 또한 `str_ireplace()` 함수를 사용하면 대소문자를 무시하고 검색과 교체를 수행할 수도 있습니다.

또한 PHP 외에도 여러 다른 언어에서도 텍스트를 검색하고 교체하는 방법이 존재합니다. 예를 들어, Javascript에서는 `replace()` 메소드를 사용하거나, Python에서는 `replace()` 함수를 사용하여 텍스트를 검색하고 교체할 수 있습니다. 따라서 다른 언어를 공부하고 있거나 앞으로 공