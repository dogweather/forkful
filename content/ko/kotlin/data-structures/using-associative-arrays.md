---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:58.077899-07:00
description: "\uCF54\uD2C0\uB9B0\uC5D0\uC11C\uC758 \uC5F0\uAD00 \uBC30\uC5F4, \uB610\
  \uB294 \uB9F5\uC740 \uD0A4-\uAC12 \uC30D\uC744 \uC800\uC7A5\uD558\uB294 \uCEEC\uB809\
  \uC158\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACE0\uC720\
  \uD55C \uD0A4\uB97C \uAE30\uBC18\uC73C\uB85C \uB370\uC774\uD130\uB97C \uD6A8\uC728\
  \uC801\uC73C\uB85C \uAD6C\uC131\uD558\uACE0 \uAC80\uC0C9\uD558\uAE30 \uC704\uD574\
  \ \uC774\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC815\uBCF4 \uAD00\uB9AC\uAC00 \uB354 \uC26C\
  \uC6CC\uC9D1\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.079713-06:00'
model: gpt-4-0125-preview
summary: "\uCF54\uD2C0\uB9B0\uC5D0\uC11C\uC758 \uC5F0\uAD00 \uBC30\uC5F4, \uB610\uB294\
  \ \uB9F5\uC740 \uD0A4-\uAC12 \uC30D\uC744 \uC800\uC7A5\uD558\uB294 \uCEEC\uB809\uC158\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACE0\uC720\uD55C\
  \ \uD0A4\uB97C \uAE30\uBC18\uC73C\uB85C \uB370\uC774\uD130\uB97C \uD6A8\uC728\uC801\
  \uC73C\uB85C \uAD6C\uC131\uD558\uACE0 \uAC80\uC0C9\uD558\uAE30 \uC704\uD574 \uC774\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC815\uBCF4 \uAD00\uB9AC\uAC00 \uB354 \uC26C\uC6CC\
  \uC9D1\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

코틀린에서의 연관 배열, 또는 맵은 키-값 쌍을 저장하는 컬렉션입니다. 프로그래머들은 고유한 키를 기반으로 데이터를 효율적으로 구성하고 검색하기 위해 이를 사용하여 정보 관리가 더 쉬워집니다.

## 어떻게 하나요?

코틀린에서 맵을 생성하고 사용하는 것은 간단합니다. 이에 대해 빠른 가이드를 제공합니다:

```Kotlin
fun main() {
    // 가변 맵 생성
    val fruits = mutableMapOf("a" to "Apple", "b" to "Banana")

    // 요소 추가
    fruits["o"] = "Orange" // 인덱싱 연산 사용
    fruits.put("g", "Grape") // put 메소드 사용

    // 요소 접근
    println(fruits["a"])  // 출력: Apple
    println(fruits["b"])  // 출력: Banana

    // 요소 제거
    fruits.remove("b")
    
    // 맵 순회
    for ((key, value) in fruits) {
        println("$key -> $value")
    }
    // 샘플 출력:
    // a -> Apple
    // o -> Orange
    // g -> Grape
}
```

## 심층 설명

코틀린의 맵은 자바와의 상호 운용성에서 직접 오며, 자바에서 맵은 컬렉션의 중요한 부분입니다. 하지만, 코틀린은 가변(`MutableMap`)과 읽기 전용(`Map`) 인터페이스를 제공함으로써 사용성을 향상시킵니다. 반면, 자바는 `Map` 인터페이스를 통합해 제공합니다. 이 구분은 컬렉션이 수정을 위해 의도되었는지 아닌지를 명확히 합니다.

코틀린 맵 구현의 중요한 세부 사항은 가변성과 불변성 맵 사이의 명시적 구분으로, 이는 언어의 불변성과 스레드 안전성에 대한 초점을 강조합니다.

맵이 매우 유용하지만, 코틀린은 각각 고유한 사용 사례가 있는 리스트와 세트와 같은 다른 컬렉션도 제공합니다. 예를 들어, 리스트는 순서를 유지하고 중복을 허용하여 인덱스로 요소에 접근하기에 이상적인 반면, 세트는 유일성을 보장하지만 순서를 유지하지 않습니다. 맵, 리스트, 또는 세트 사용 사이의 선택은 키 기반 접근이나 순서 유지와 같은 애플리케이션의 특정 요구사항에 따라 달라집니다.

더 나은 대안으로, 특히 대규모 컬렉션에서 성능이 중요한 경우, 동시 접근이나 정렬과 같은 특정 사용 사례에 최적화된 외부 라이브러리가 제공하는 전문화된, 효율적인 데이터 구조를 사용하는 것을 고려하세요.
