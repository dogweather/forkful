---
title:                "랜덤 숫자 생성하기"
html_title:           "PHP: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성해야 하는 이유는 다양합니다. 일반적으로 데이터를 무작위로 선택하거나 추출하고자 할 때, 그리고 무작위로 변형한 값들을 사용하는 시뮬레이션 작업을 할 때 랜덤 숫자가 필요합니다.

## 방법

PHP에서 랜덤 숫자를 생성하는 방법은 다양합니다. 아래는 가장 일반적인 방법입니다.

```PHP
// 1과 10 사이의 임의의 정수를 생성
$random_number = rand(1, 10);
echo $random_number;
// 예시 출력: 7

// 0과 1 사이의 임의의 실수를 생성
$random_float = rand() / getrandmax();
echo $random_float;
// 예시 출력: 0.626356071226

// 주어진 배열에서 무작위로 하나의 요소 선택
$fruits = ['apple', 'orange', 'banana', 'kiwi'];
$random_fruit = array_rand($fruits);
echo $fruits[$random_fruit];
// 예시 출력: banana
```

## 깊게 들어가보기

PHP에서 랜덤 숫자를 생성하는 함수는 크게 두 가지가 있습니다. 첫 번째는 `rand()` 함수로, 주어진 두 개의 매개변수 사이의 임의의 정수를 반환합니다. 이때 매개변수는 양의 정수이어야 합니다. 두 번째는 `getrandmax()` 함수로, 현재 시스템에서 생성될 수 있는 가장 큰 수를 반환합니다. 또한, 랜덤 숫자를 생성할 때 주의해야 할 점은 시드(Seed) 값을 설정하는 것입니다. 시드 값이 같다면 같은 랜덤 숫자를 반복적으로 생성하게 됩니다. 따라서 시드 값은 가능한 한 고유한 값이 되어야 합니다.

## 관련 링크들

- [PHP 공식 문서 - 랜덤 함수들](https://www.php.net/manual/en/function.rand.php)
- [컴퓨터의 랜덤과 유사성](https://www.random.org/randomness/)
- [파이썬에서 랜덤 숫자 생성하기](https://www.digitalocean.com/community/tutorials/how-to-use-the-python-random-module-to-generate-numbers)