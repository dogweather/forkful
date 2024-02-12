---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:11:10.521242-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇을 왜?
코드를 함수로 조직화하는 것은 정의된 목적을 가진 재사용 가능한 블록으로 코드를 나누는 것에 대한 것입니다. 이는 코드를 깔끔하게 유지하고, 중복을 방지하고, 디버깅을 쉽게 하기 위해 수행됩니다.

## 어떻게:
사용자에게 인사하는 반복적인 코드가 있다고 상상해 봅시다. 대신, 이를 `greet_user`라는 함수로 감쌀 것입니다:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

출력 결과:
```
Hello, Alice!
Hello, Bob!
```

이제, 인사하는 동일한 코드를 매번 다시 작성하지 않고 언제든 사용할 수 있는 편리한 도구가 생겼습니다.

## 심층 탐구
함수는 '50년대 FORTRAN 초기부터 프로그래밍에 있었습니다. 구조화된 프로그래밍의 기초이며 모듈성과 분리에 관한 것입니다. 대안이라면? 객체 지향을 활용하여 클래스와 메서드(기능적으로 멋진 정장을 입은 함수들)에 대해 이야기할 수 있습니다. PHP의 경우, 구현 세부 사항에는 매개변수의 기본값 지정, 입력에 대한 타입 힌팅, 배열이나 PHP 7.1부터는 리스트를 사용하여 여러 값을 반환할 수 있음을 포함합니다.

여기 타입 선언과 기본값을 사용한 현대적인 변주가 있습니다:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4는 배열 연산에서 일반적으로 사용되는 간결한 일 줄 함수를 작성하는 데 도움이 되는 화살표 함수도 도입했습니다:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

출력 결과:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## 참조
- [함수에 대한 PHP 매뉴얼](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: 올바른 방법 - 함수](https://phptherightway.com/#functions)
- [PHP 7.4 화살표 함수에 대해 알아보기](https://stitcher.io/blog/short-closures-in-php)
