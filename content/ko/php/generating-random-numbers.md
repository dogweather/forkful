---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

랜덤 숫자 생성은 예측이 불가능한 숫자를 코드에서 생성하는 것입니다. 이는 테스트 데이터 만들기, 비밀번호 생성, 게임 내 랜덤 이벤트 등 다양한 프로그래밍 작업에서 필수적입니다.

## 사용법:

아래 코드 블록에서 랜덤 숫자 생성을 보여줍니다.

```PHP
<?php
  $randomNum = rand(1, 10);
  echo $randomNum;
?>
```
난수 생성 함수`rand()`는 입력한 두 숫자 사이의 랜덤 값을 반환합니다.
예를 들어, 위의 코드는 1부터 10까지의 숫자 중 랜덤한 숫자를 출력합니다.

## 깊게 알아보기

랜덤 숫자 생성은 컴퓨터 과학 초창기부터 있었으며, 주로 시뮬레이션, 암호화, 알고리즘 등에서 사용됩니다. `rand()` 함수는 인자로 받은 두 숫자 사이에서 정수를 생성하지만, 실제로는 유사 난수(Pseudo-random)를 생성해 내는데, 시작점(seed)을 바탕으로 숫자열이 만들어집니다.

PHP에는 `rand()` 외에도 `mt_rand()`, `random_int()` 등 여러 가지 랜덤 숫자 생성 함수가 있습니다. `mt_rand()`는 메르센 트위스터 알고리즘을 사용하며, `random_int()`는 암호학적으로 안전한 정수 난수를 생성하는데 사용됩니다.

## 참고 

1. PHP Manual: rand() - https://www.php.net/manual/en/function.rand.php
2. PHP Manual: mt_rand() - https://www.php.net/manual/en/function.mt-rand.php
3. PHP Manual: random_int() - https://www.php.net/manual/en/function.random-int.php