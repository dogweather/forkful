---
title:                "리팩터링"
date:                  2024-01-26T01:44:27.447475-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩터링"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩토링은 기존 코드의 구조, 가독성, 성능을 개선하기 위해 코드를 조정하는 과정이지만, 그 외부 행위는 변경하지 않습니다. 프로그래머들은 코드를 더 유지보수하기 쉽게, 새로운 기능을 추가하기 쉽게 만들고, 버그를 더 쉽게 찾아내고 수정하기 위해 리팩토링합니다.

## 방법:
다음은 흔히 보이는 코드 스멜(code smell)과 그것의 리팩토링 버전을 보여주는 코틀린(Kotlin) 코드 조각입니다. 우리는 너무 많은 일을 하고 있는 코드 조각으로 시작합니다:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // 주문 총액 계산하기
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // 할인 적용
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // 더 많은 처리...
    }
}
```

더 나은 가독성과 염려사항의 분리를 위해 리팩토링된 버전:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

기능은 변경하지 않았기 때문에 여기에 샘플 출력은 없지만, 코드의 가독성과 유지보수성이 크게 향상되었습니다!

## 심층 분석
리팩토링이라는 개념은 프로그래밍이 시작된 이래로 존재해 왔지만, 특히 1990년대, 마틴 파울러(Martin Fowler)가 1999년에 "리팩토링: 기존 코드의 설계 개선(Refactoring: Improving the Design of Existing Code)"을 출판한 이후로 정말로 한 분야로 떠올랐습니다. 이 책은 관행에 이름을 부여하고, 여기에 적용되는 조직된 방법을 정의했으며, 리팩토링 기법의 카탈로그를 포함했습니다.

리팩토링과 대안을 비교: 코드를 처음부터 다시 작성할 수 있습니다(위험하고 시간이 많이 걸림) 또는 단순히 추가적인 변경만 할 수 있습니다(소프트웨어의 비대화와 잠재적인 기술 부채를 초래함). 리팩토링은 이상적인 중간지점을 찾습니다—현대화하고 정리하면서 위험을 낮게 유지합니다.

구현 측면에서는 리팩토링을 시작하기 전에 프로그램의 동작을 우발적으로 변경하지 않도록 하기 위해서 견고한 테스트 세트를 갖추는 것이 필수적입니다. 많은 현대 IDE(코틀린용 IntelliJ 포함)는 변수 이름 변경, 메서드 추출 등을 자동화하는 리팩토링 도구를 제공하여 프로세스를 가속화하고 오류를 줄일 수 있습니다.

## 참고 문헌
- 마틴 파울러(Martin Fowler)의 "리팩토링: 기존 코드의 설계 개선(Refactoring: Improving the Design of Existing Code)" (이 주제에 대한 기초 작업)
- 코틀린 문서의 코딩 규칙: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) ('코틀린 방식'의 깨끗한 코드를 이해하기 위해)
- IntelliJ IDEA에서의 JetBrains 리팩토링 지원: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (실용적인 리팩토링 도구 사용을 위해)
- 구글의 대규모 리팩토링 가이드: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (더 큰 리팩토링 과제를 다룰 때의 통찰력을 위해)