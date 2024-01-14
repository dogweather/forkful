---
title:                "PHP: 랜덤 숫자 생성하기"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

난수 생성에 참여할 이유는 무엇인가요? 

난수 생성은 프로그래밍에서 매우 중요한 역할을 합니다. 난수는 보안, 시뮬레이션 및 게임 등 다양한 분야에서 사용됩니다. 프로그래밍에서 가장 흥미로운 부분 중 하나는 난수를 생성하는 과정이며, 이를 통해 우리는 다양한 수학적 과정과 알고리즘을 배울 수 있습니다. 따라서 난수 생성은 프로그래밍을 배우는 과정에서 중요한 부분입니다.

## 방법

```PHP
<?php
// rand() 함수를 사용하여 1부터 10까지의 난수 생성
echo rand(1, 10) . "\n";
// 출력 결과는 예: 8
?>
```

PHP에서 난수를 생성하는 가장 간단한 방법은 `rand()` 함수를 사용하는 것입니다. 이 함수는 두 개의 매개변수를 가지며, 첫 번째 매개변수는 난수의 최소값을, 두 번째 매개변수는 최대값을 나타냅니다. 위 예제에서는 1부터 10까지의 난수를 생성했습니다. `rand()` 함수 외에도, `mt_rand()` 함수와 `random_int()` 함수를 사용하여 난수를 생성할 수 있습니다.

## 딥 다이브

난수를 생성하는 알고리즘 중 하나는 선형 합동 발생기 (Linear Congruential Generator)입니다. 이 알고리즘은 현재 시간 및 이전에 생성된 난수를 사용하여 새로운 난수를 생성합니다. 또 다른 알고리즘은 메르센 트위스터 (Mersenne Twister) 알고리즘입니다. 이 알고리즘은 최근에 가장 널리 사용되는 난수 생성 알고리즘 중 하나입니다. 난수 생성은 간단해 보이지만, 컴퓨터 과학 분야에서 여러 가지 문제와 연관되어 있습니다. 따라서 프로그래머는 난수를 사용할 때 항상 주의를 기울여야 합니다.

## 참고 자료

- [PHP 공식 문서 - 난수 생성](https://www.php.net/manual/en/function.rand.php)
- [선형 합동 발생기 (Wikipedia)](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [메르센 트위스터 (Wikipedia)](https://en.wikipedia.org/wiki/Mersenne_Twister)