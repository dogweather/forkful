---
title:                "리팩토링"
date:                  2024-01-26T01:18:44.543939-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/refactoring.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
리팩토링은 코드를 재작업하여 보다 깨끗하고 유지관리가 용이하면서도 외부 행동을 변경하지 않게 만드는 과정입니다. 프로그래머들은 가독성을 향상시키고, 복잡성을 줄이며, 코드베이스를 미래의 업데이트나 기능 추가에 더 용이하게 만들기 위해 리팩토링을 합니다.

## 방법:
여러 함수에 걸쳐 반복적인 계산이나 문자열 조작을 하는 코드 덩어리가 있다고 가정해 보겠습니다. 그것은 리팩토링을 위한 주요 대상입니다. 여기 타입 안전성과 불변성에 중점을 둔 Gleam을 사용하여 리팩토링 전과 후의 예가 있습니다:

```gleam
// 리팩토링 전
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("The area is \(area)")
}

// 리팩토링 후
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("The area is \(area)")
}

// 코드의 다른 부분에서, 이렇게 print_area를 호출할 것입니다:
print_area(calculate_area(10, 20))
```

샘플 출력:
```
The area is 200
```

리팩토링을 통해, 우리는 `print_area`가 단지 출력에만 집중하도록 만들었으며, 계산은 다른 곳에서 처리되므로 코드를 더 모듈화하고 재사용하거나 테스트하기 쉽게 만들었습니다.

## 심층 분석
리팩토링이라는 개념은 프로그래밍 자체만큼 오래되었으며—코드를 다시 방문하고 정리하는 것은 좋은 집안일의 일부입니다. 리팩토링의 현대적인 공식화와 오늘날 사용되는 많은 기법과 패턴은 1999년에 출판된 마틴 파울러의 기념비적인 책 "Refactoring: Improving the Design of Existing Code"으로 거슬러 올라갈 수 있습니다.

Gleam 생태계에서 리팩토링은 특정 고려 사항이 있습니다. 가장 중요한 것 중 하나는 컴파일 시 강력한 타입 검사로, 물건을 옮기는 동안 조기에 실수를 잡는 데 도움이 될 수 있습니다. Gleam의 패턴 매칭과 불변성 기능은 또한 리팩토링의 주요 목표 중 하나인 보다 명확하고 간결한 코드를 작성하도록 안내할 수 있습니다.

리팩토링의 대안은 처음부터 코드를 다시 쓰거나 빠른 수정으로 코드를 패치하는 것을 포함할 수 있습니다. 그러나, 리팩토링은 일반적으로 새로운 버그를 도입하지 않고 기존 코드를 개선하는 가장 안전하고 효율적인 접근 방식으로, 점진적이고 잘 강조된, 행동을 보존하는 변환을 포함합니다.

## 참고 자료
- 마틴 파울러의 "Refactoring" 책: https://martinfowler.com/books/refactoring.html
- 추가 문서와 예제가 있는 Gleam 언어 웹사이트: https://gleam.run/
- 다양한 언어에서 적용 가능한 기본 원리에 대한 "Refactoring: Improving the Design of Existing Code" by Martin Fowler: https://martinfowler.com/books/refactoring.html
