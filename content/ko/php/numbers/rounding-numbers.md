---
date: 2024-01-26 03:45:59.627603-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uC18C\uC218\uC810\uC744 \uC815\uD574\uC9C4 \uC815\uBC00\uB3C4\uAE4C\uC9C0 \uC790\
  \uB974\uB294 \uAC83\uC744 \uC758\uBBF8\uD558\uBA70, \uC885\uC885 \uC815\uC218\uB85C\
  \ \uCC98\uB9AC\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACC4\
  \uC0B0\uC744 \uB2E8\uC21C\uD654\uD558\uAE30, \uC131\uB2A5\uC744 \uD5A5\uC0C1\uC2DC\
  \uD0A4\uAE30 \uB610\uB294 \uCD9C\uB825\uC744 \uC0AC\uC6A9\uC790 \uCE5C\uD654\uC801\
  \uC73C\uB85C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uBC18\uC62C\uB9BC\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.271741-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740 \uC18C\
  \uC218\uC810\uC744 \uC815\uD574\uC9C4 \uC815\uBC00\uB3C4\uAE4C\uC9C0 \uC790\uB974\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD558\uBA70, \uC885\uC885 \uC815\uC218\uB85C \uCC98\
  \uB9AC\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACC4\uC0B0\
  \uC744 \uB2E8\uC21C\uD654\uD558\uAE30, \uC131\uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\
  \uAE30 \uB610\uB294 \uCD9C\uB825\uC744 \uC0AC\uC6A9\uC790 \uCE5C\uD654\uC801\uC73C\
  \uB85C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uBC18\uC62C\uB9BC\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?
숫자를 반올림한다는 것은 소수점을 정해진 정밀도까지 자르는 것을 의미하며, 종종 정수로 처리됩니다. 프로그래머들은 계산을 단순화하기, 성능을 향상시키기 또는 출력을 사용자 친화적으로 만들기 위해 반올림을 사용합니다.

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
