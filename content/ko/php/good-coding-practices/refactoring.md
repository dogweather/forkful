---
date: 2024-01-26 01:49:56.250165-07:00
description: "\uBC29\uBC95: \uD074\uB798\uC2DD\uD55C PHP \uC2A4\uB2C8\uD3AB\uC744\
  \ \uAC00\uC838\uC640\uC11C \uB9AC\uD329\uD130\uB9C1 \uB9C8\uBC95\uC744 \uC801\uC6A9\
  \uD574\uBD05\uC2DC\uB2E4. \uB9AC\uD329\uD130\uB9C1 \uC804, \uC6B0\uB9AC\uC758 \uCF54\
  \uB4DC\uB294 \uC774\uB807\uAC8C \uBCF4\uC77C \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.369126-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB798\uC2DD\uD55C PHP \uC2A4\uB2C8\uD3AB\uC744 \uAC00\uC838\uC640\
  \uC11C \uB9AC\uD329\uD130\uB9C1 \uB9C8\uBC95\uC744 \uC801\uC6A9\uD574\uBD05\uC2DC\
  \uB2E4."
title: "\uB9AC\uD329\uD130\uB9C1"
weight: 19
---

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
