---
title:                "난수 생성하기"
date:                  2024-01-20T17:50:02.106007-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가와 왜 그렇게 하는가?)

랜덤 번호 생성은 예측할 수 없는 숫자를 만드는 것입니다. 프로그래머는 게임, 보안, 데이터 분석 등과 같은 상황에서 사용합니다.

## How to (방법)

PHP에서 랜덤 번호를 생성하는 기본 함수는 `rand()`와 `mt_rand()`입니다. `mt_rand()`가 더 나은 난수 발생기를 사용하기 때문에 빠르고, 더 좋은 결과를 제공합니다.

```php
<?php
// 기본적인 난수 생성
$randomNumber = rand(0, 100);
echo $randomNumber;

// 좀 더 나은 난수 생성
$betterRandomNumber = mt_rand(0, 100);
echo $betterRandomNumber;

// PHP 7 이상부터 사용 가능한 random_int()
$secureRandomNumber = random_int(0, 100);
echo $secureRandomNumber;
?>
```

이 코드는 0부터 100까지의 랜덤 숫자를 출력합니다. `random_int()` 함수는 암호화에 적합한 강력한 난수를 제공합니다.

## Deep Dive (심도 있는 정보)

PHP의 랜덤 함수는 여러 단계로 발전해왔습니다. 초기에는 `rand()`만 사용했지만, 알고리즘이 예측 가능해서 보안에 취약했습니다. 그래서 나온 것이 `mt_rand()`, Mersenne Twister 알고리즘을 사용하는 함수입니다. `random_int()`는 PHP 7부터 추가되어 암호학적으로 안전한 난수를 생성할 수 있게 해줍니다.

대안으로 `openssl_random_pseudo_bytes()` 함수를 사용할 수도 있지만, 이는 바이트 문자열을 반환하므로 숫자 형태로 직접 사용하려면 변환이 필요합니다. 난수 생성에는 시스템 상태, 시간, 기타 변할 수 있는 값들을 이용하여 ‘씨앗(seed)’을 제공하고 이를 통해 예측할 수 없는 결과를 도출합니다. 보안을 위해 최신 PHP 버전에서는 시스템 수준의 난수 생성기를 이용하는 것을 추천합니다.

## See Also (참고 자료)

- PHP Manual on `rand()`: https://www.php.net/manual/en/function.rand.php
- PHP Manual on `mt_rand()`: https://www.php.net/manual/en/function.mt-rand.php
- PHP Manual on `random_int()`: https://www.php.net/manual/en/function.random-int.php
- Mersenne Twister 알고리즘 소개: https://en.wikipedia.org/wiki/Mersenne_Twister
- 암호학적으로 안전한 난수 생성에 대한 설명: https://www.php.net/manual/en/function.random-bytes.php