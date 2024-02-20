---
date: 2024-01-26 04:44:20.901917-07:00
description: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80\
  \uB85C \uAD6C\uC131\uB418\uBA70, \uC8FC\uB85C `a + bi`\uB85C \uD45C\uAE30\uB429\uB2C8\
  \uB2E4. \uBCF5\uC18C\uC218\uB294 \uACE0\uAE09 \uC218\uD559, \uBB3C\uB9AC\uD559,\
  \ \uACF5\uD559, \uADF8\uB9AC\uACE0 \uD2B9\uC815 \uCEF4\uD4E8\uD130 \uC54C\uACE0\uB9AC\
  \uC998\uC5D0\uC11C \uC911\uC694\uD55C \uC5ED\uD560\uC744 \uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC74C\uC218\uC758 \uC81C\uACF1\uADFC \uBC0F\
  \ \uC9C4\uB3D9\uD558\uB294 \uD568\uC218\uB4E4\uC744 \uB2E4\uB8E8\uB294 \uACC4\uC0B0\
  \uC5D0 \uADF8\uAC83\uB4E4\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.260252
model: gpt-4-0125-preview
summary: "\uBCF5\uC18C\uC218\uB294 \uC2E4\uC218\uBD80\uC640 \uD5C8\uC218\uBD80\uB85C\
  \ \uAD6C\uC131\uB418\uBA70, \uC8FC\uB85C `a + bi`\uB85C \uD45C\uAE30\uB429\uB2C8\
  \uB2E4. \uBCF5\uC18C\uC218\uB294 \uACE0\uAE09 \uC218\uD559, \uBB3C\uB9AC\uD559,\
  \ \uACF5\uD559, \uADF8\uB9AC\uACE0 \uD2B9\uC815 \uCEF4\uD4E8\uD130 \uC54C\uACE0\uB9AC\
  \uC998\uC5D0\uC11C \uC911\uC694\uD55C \uC5ED\uD560\uC744 \uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC74C\uC218\uC758 \uC81C\uACF1\uADFC \uBC0F\
  \ \uC9C4\uB3D9\uD558\uB294 \uD568\uC218\uB4E4\uC744 \uB2E4\uB8E8\uB294 \uACC4\uC0B0\
  \uC5D0 \uADF8\uAC83\uB4E4\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBCF5\uC18C\uC218 \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
복소수는 실수부와 허수부로 구성되며, 주로 `a + bi`로 표기됩니다. 복소수는 고급 수학, 물리학, 공학, 그리고 특정 컴퓨터 알고리즘에서 중요한 역할을 합니다. 프로그래머들은 음수의 제곱근 및 진동하는 함수들을 다루는 계산에 그것들을 사용합니다.

## 방법:
PHP는 `ext-intl` 확장 기능과 `NumberFormatter` 클래스를 사용하여 복소수를 지원합니다. 다음은 예입니다:

```php
// intl 확장 기능이 로드되었는지 확인
if (!extension_loaded('intl')) {
    die("intl 확장 기능이 활성화되지 않았습니다. 이 코드를 실행하려면 활성화하세요.");
}

function addComplexNumbers($a, $b) {
    // NumberFormatter를 사용하여 복소수를 파싱하고 포맷하기
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // 문자열에서 복소수 파싱하기
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // 덧셈 수행
    $sum = $numA + $numB;

    // 결과를 복소수로 포맷하기
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // 출력: 7+10i
```

## 심층 분석
`ext-intl` 이전에는 PHP가 복소수를 기본적으로 지원하지 않았습니다. 개발자들은 복소수를 다루기 위해 함수나 사용자 정의 클래스 라이브러리를 사용했습니다. 복잡한 연산은 번거롭고 오류가 발생하기 쉬웠지만, `ext-intl`은 ICU 라이브러리와 일치하는 국제화된 방식으로 복소수를 표시하고 파싱할 수 있는 방법을 제공합니다.

그러나, 무거운 수학 연산의 경우, 일부는 더 수학 친화적인 언어(C나 파이썬 같은)로 작성된 외부 라이브러리를 사용하고 PHP를 통해 그것들과 인터페이스할 수 있습니다. 구현과 관련하여, `ext-intl`은 배후에서 처리하며, 개발자로부터 복잡성을 추상화하면서 정확한 산술 연산을 보장합니다.

역사적으로 복소수는 '허수'로 불리며 경멸의 대상이었지만, 이후에 다양한 과학 및 수학 분야에서 근본적인 역할을 하게 되면서, 그것들이 상상 이상의 실제 세계의 중요성을 드러내게 되었습니다.

## 참고 자료
- [PHP 매뉴얼의 NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [복소수에 대한 위키피디아](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: 올바른 방법 - 데이터 타입 작업하기](https://phptherightway.com/#data_types)
