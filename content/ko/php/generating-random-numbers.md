---
title:                "랜덤 숫자 생성"
html_title:           "PHP: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이고, 왜?
난수 생성이란 무엇일까요? 이것은 프로그래머들이 왜 이 작업을 하는지 궁금한 분들도 있을 것입니다. 간단하게 말씀드리자면, 난수 생성은 우리가 원할 때마다 랜덤한 숫자를 만들기 위한 방법입니다. 프로그래머들은 이를 사용하여 다양한 기능을 실행할 수 있습니다.

## 방법:
```PHP
// 1에서 10까지의 랜덤한 숫자 생성
echo mt_rand(1, 10);

// 0에서 100까지의 랜덤한 소수점 숫자 생성
echo rand(0, 100) / 10;

// 배열에서 랜덤한 값을 선택
$fruits = array("apple", "orange", "banana", "grape");
echo $fruits[array_rand($fruits)];
```
출력:
```
7
8.5
orange
```

## 깊이 파헤치기:
난수 생성은 프로그래밍에서 매우 중요한 개념입니다. 이는 무작위성을 쉽게 구현할 수 있는 방법이기 때문입니다. 예전에는 주사위나 카드를 사용하여 난수를 생성했지만, 프로그래밍 언어에서는 더 간단한 방법을 제공합니다. PHP에서는 `mt_rand()`와 `rand()` 함수를 사용하여 랜덤한 값을 생성할 수 있습니다. 또한 `array_rand()` 함수를 사용하면 배열에서 랜덤한 값을 선택할 수 있습니다. 다른 언어에서도 비슷한 함수를 사용할 수 있으니 참고하시기 바랍니다.

## 참고:
- [PHP 공식 문서 - 난수 생성 함수](https://www.php.net/manual/en/function.mt-rand.php)
- [Wikipedia - 난수 생성](https://ko.wikipedia.org/wiki/%EB%82%9C%EC%88%98_%EC%83%9D%EC%84%B1)