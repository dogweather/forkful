---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:12:31.842093-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
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
