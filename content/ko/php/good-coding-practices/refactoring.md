---
date: 2024-01-26 01:49:56.250165-07:00
description: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80\
  \ \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\uC11C \uC7AC\uAD6C\
  \uC131\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758 \uBE44\uAE30\uB2A5\uC801 \uC18D\
  \uC131\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uB9AC\uD329\uD130\uB9C1\uC744\
  \ \uD558\uC5EC \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0, \uD6A8\uC728\uC801\
  \uC774\uBA70, \uC720\uC9C0 \uBCF4\uC218\uD558\uAE30 \uC27D\uAC8C \uB9CC\uB4ED\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.369126-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC73C\uBA74\uC11C \uC7AC\uAD6C\uC131\
  \uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD130\uB9C1"
weight: 19
---

## 무엇 & 왜?

리팩터링은 기존 컴퓨터 코드의 구조를 변경하지 않고 외부 동작을 변경하지 않으면서 재구성하는 과정입니다. 프로그래머들은 소프트웨어의 비기능적 속성을 개선하기 위해 리팩터링을 하여 코드를 더 깨끗하고, 효율적이며, 유지 보수하기 쉽게 만듭니다.

## 방법:

클래식한 PHP 스니펫을 가져와서 리팩터링 마법을 적용해봅시다.

리팩터링 전, 우리의 코드는 이렇게 보일 수 있습니다:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

하지만 이 코드를 리팩터링하여 명확성과 모듈성을 개선할 수 있습니다:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
`printOrderDetails` 함수를 더 작은 함수로 분해함으로써, 우리의 코드는 더 읽기 쉽고 디버그하기 쉬워집니다.

## 심층 분석

리팩터링은 1990년대 초반 smalltalk 프로그래밍 커뮤니티에서 기원했으며, Martin Fowler의 주요 서적 "Refactoring: Improving the Design of Existing Code" (1999)에 의해 더욱 대중화되었습니다. 리팩터링은 모든 프로그래밍 언어에 적용될 수 있지만, PHP의 동적 특성은 독특한 도전과 기회를 제공합니다.

리팩터링 대안으로는 코드를 처음부터 다시 작성하는 것이 있을 수 있으며, 이는 종종 더 위험하고 시간이 많이 소요됩니다. PHP 생태계에서는 PHPStan과 Rector와 같은 도구가 각각 자동으로 일부 리팩터링 작업을 탐지하고 수행할 수 있습니다. 구현 측면에서, 리팩터링을 작게 유지하고 단위 테스트로 광범위하게 테스트하는 것은 버그를 도입하지 않고 성공적인 리팩터링을 보장하는 핵심 관행입니다.

## 참고 자료
- Martin Fowler의 리팩터링 책: https://martinfowler.com/books/refactoring.html
- PHPStan, PHP 정적 분석 도구: https://phpstan.org/
- Rector, PHP 코드 자동 리팩터링 도구: https://getrector.org/
- PHPUnit을 사용한 PHP 단위 테스팅: https://phpunit.de/
