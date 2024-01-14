---
title:                "PHP: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜

PHP에서 임의의 숫자를 생성하는 것에 대해 관심이 생길 수 있습니다. 이를 통해 새로운 콘텐츠를 만들거나 데이터를 랜덤하게 처리할 수 있습니다.

## 방법

PHP에서 임의의 숫자를 생성하는 방법은 매우 간단합니다. 다음 코드를 사용하면 됩니다.

```PHP
// 랜덤한 숫자를 생성하는 예제
$number = rand(1, 10); // 1에서 10 사이의 숫자를 랜덤하게 선택

// 랜덤한 숫자와 문자열을 조합하는 예제
$randomString = "Your lucky number for today is " . rand(1, 100); // 1에서 100 사이의 숫자를 랜덤하게 선택하여 문자열과 조합
echo $randomString; // 예시 출력: Your lucky number for today is 42
```

위의 예제는 `rand()` 함수를 사용하여 랜덤한 숫자를 생성하는 방법을 보여줍니다. 첫 번째 매개변수는 가장 작은 숫자이고, 두 번째 매개변수는 가장 큰 숫자입니다. 따라서 위의 예제에서는 1부터 10까지의 숫자를, 또 다른 예제에서는 1부터 100까지의 숫자를 랜덤하게 선택합니다.

## 딥 다이브

PHP에서 임의의 숫자를 생성하는 방법은 여러 가지가 있습니다. `rand()` 함수 외에도 `mt_rand()` 함수, `random_int()` 함수 등이 있습니다. 또한 각 함수마다 다양한 매개변수를 사용할 수 있고, 보안 측면에서 추천되는 함수가 있으므로 사용하기 전에 알아두는 것이 좋습니다.

또한, PHP에서는 버전에 따라 난수 생성 방식이 달라질 수 있으므로 원하는 대로 숫자를 생성하지 못하는 경우가 있을 수 있습니다. 이 때는 `phpinfo()` 함수를 사용하여 PHP 버전과 난수 생성 방식을 확인한 뒤 적절한 함수를 선택하여 사용해야 합니다.

# 참고 자료

- [PHP 공식 문서 - 임의의 숫자 생성](https://www.php.net/manual/en/function.rand.php)
- [Randomness in PHP - Secure random numbers](https://paragonie.com/blog/2016/05/how-generate-secure-random-numbers-in-various-programming-languages#advice-php)
- [Learn PHP with Codecademy - Random Numbers](https://www.codecademy.com/learn/learn-php/modules/introduction-to-php/cheatsheet)