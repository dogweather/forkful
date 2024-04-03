---
date: 2024-01-26 03:45:59.627603-07:00
description: "\uBC29\uBC95: PHP\uB294 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294\
  \ \uBA87 \uAC00\uC9C0 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4: `round()`,\
  \ `ceil()`, \uADF8\uB9AC\uACE0 `floor()`. \uB2E4\uC74C\uC740 \uADF8\uB4E4\uC774\
  \ \uC791\uB3D9\uD558\uB294 \uBC29\uC2DD\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.349708-06:00'
model: gpt-4-0125-preview
summary: "PHP\uB294 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uBA87 \uAC00\
  \uC9C0 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
PHP는 숫자를 반올림하는 몇 가지 방법을 제공합니다: `round()`, `ceil()`, 그리고 `floor()`. 다음은 그들이 작동하는 방식입니다:

```php
echo round(3.14159);   // 3을 반환
echo round(3.14159, 2); // 3.14를 반환

echo ceil(3.14159);    // 4를 반환, 항상 올림

echo floor(3.14159);   // 3을 반환, 항상 내림
```

## 심도 있게 보기
숫자를 반올림하는 것은 수학과 계산에서 고대 시대부터 필수적이었습니다. 비현실적인 무한 소수점을 다루기 위해서죠. PHP에서, `round()`는 정밀도 매개변수와 모드를 받을 수 있으며, 이는 그 행동에 영향을 미칩니다 - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN` 등은 ".5" 시나리오를 만났을 때 어떻게 행동할지를 정의합니다. 정밀도는 반올림이 법적으로 규제될 수 있는 금융 애플리케이션에서 중요한데, 이는 코드에서 `round()`가 어떻게 구현되는지에 영향을 미칩니다.

내장 함수에 대한 대안으로는 사용자 정의 반올림 방법이나 임의 정밀도 산술을 위한 BC Math 함수가 있으며, 이는 더 많은 제어가 필요하거나 네이티브 정확도가 부족할 수 있는 매우 큰 숫자를 다루는 시나리오에 유용합니다.

## 참고하기
PHP 매뉴얼에서 더 탐색하기:
- [PHP `round` 함수](https://php.net/manual/en/function.round.php)
- [PHP `ceil` 함수](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` 함수](https://php.net/manual/en/function.floor.php)
- [임의 정밀도 산술을 위한 BC Math](https://php.net/manual/en/book.bc.php)
