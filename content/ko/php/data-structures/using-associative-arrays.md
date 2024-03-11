---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:31.842093-07:00
description: "PHP\uC758 \uC5F0\uAD00 \uBC30\uC5F4\uC740 \uAC01 \uC694\uC18C\uB97C\
  \ \uC22B\uC790\uAC00 \uC544\uB2CC \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uD0A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC561\uC138\uC2A4\uD560 \uC218 \uC788\uB294\
  \ \uCD08\uAC15\uB825 \uB9AC\uC2A4\uD2B8\uC640 \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB370\uC774\uD130\
  \uB97C \uBCF4\uB2E4 \uC9C1\uAD00\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uACE0 \uC870\
  \uC791\uD558\uC5EC \uCF54\uB4DC\uB97C \uB354 \uC27D\uAC8C \uC77D\uACE0 \uC720\uC9C0\
  \uBCF4\uC218\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.268614-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC758 \uC5F0\uAD00 \uBC30\uC5F4\uC740 \uAC01 \uC694\uC18C\uB97C \uC22B\
  \uC790\uAC00 \uC544\uB2CC \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uD0A4\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC561\uC138\uC2A4\uD560 \uC218 \uC788\uB294 \uCD08\
  \uAC15\uB825 \uB9AC\uC2A4\uD2B8\uC640 \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB370\uC774\uD130\uB97C\
  \ \uBCF4\uB2E4 \uC9C1\uAD00\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uACE0 \uC870\uC791\
  \uD558\uC5EC \uCF54\uB4DC\uB97C \uB354 \uC27D\uAC8C \uC77D\uACE0 \uC720\uC9C0\uBCF4\
  \uC218\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

PHP의 연관 배열은 각 요소를 숫자가 아닌 사람이 읽을 수 있는 키를 사용하여 액세스할 수 있는 초강력 리스트와 같습니다. 프로그래머는 이를 사용하여 데이터를 보다 직관적으로 저장하고 조작하여 코드를 더 쉽게 읽고 유지보수할 수 있습니다.

## 방법:

PHP에서 연관 배열을 생성하고 사용하는 것은 매우 간단합니다. 여기 빠른 개요가 있습니다:

```PHP
<?php
// 연관 배열 생성
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// 또는 단축 배열 구문
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// 키를 사용하여 값에 액세스
echo "Name: " . $person["name"] . "\n";
echo "Age: " . $person["age"] . "\n";
echo "Email: " . $person["email"] . "\n";

// 값을 수정
$person["age"] = 31;

// 새 키-값 쌍 추가
$person["country"] = "USA";

// 연관 배열을 반복
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// 출력
// Name: John Doe
// Age: 31
// Email: john@example.com
// country: USA
?>
```

키는 어떤 문자열이든 될 수 있어, 이러한 키를 사용하여 원소에 액세스할 수 있으므로, 의미가 덜하고 기억하기 어려운 숫자 인덱스보다는 사용이 편리합니다.

## 심층 분석

PHP의 연관 배열은 내부적으로 해시 테이블을 사용하여 구현되어 있으며, 이는 키로 요소에 대한 매우 빠른 액세스를 제공하여 많은 작업에 대해 매우 효율적입니다. 이러한 효율성과 사용의 용이성은 PHP 프로그래밍의 핵심 요소로 만듭니다.

역사적으로 PHP의 배열(인덱스 배열 및 연관 배열 모두)은 매우 유연하여 리스트, 스택, 큐 등으로 활용될 수 있었습니다. 하지만, 이러한 유연성은 주의 깊게 사용하지 않으면 때때로 비효율적인 코드로 이어질 수 있습니다.

최근, PHP의 객체 지향 프로그래밍에서의 개선으로 일부 개발자는 복잡하거나 상호 연관된 데이터 세트에 대해 구조화된 데이터에 대해 객체를 사용하는 것을 선호합니다. 클래스를 사용하면 더 나은 캡슐화와 추상화를 제공하고 코드를 더 쉽게 테스트할 수 있으며 의도를 명확하게 할 수 있습니다. 그러나 간단한 키-값 저장과 직관적인 구문으로 인한 간단한 데이터 조작 시나리오의 경우, 연관 배열은 여전히 우수한 선택입니다.
