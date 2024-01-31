---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:11:10.227123-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

연관 배열은 Go에서는 맵(map)으로 알려져 있으며, 키-값 쌍을 사용하여 데이터를 저장하고 접근할 수 있게 해줍니다. 유니크한 키로 값을 신속하게 찾아볼 수 있어 컬렉션 관리에 필수적이며, 프로그램에서 데이터 조작과 검색을 간단하게 만들어 줍니다.

## 사용 방법:

Go에서 맵을 사용하는 것은 직관적입니다. 시작하는 데 도움이 되는 간단한 안내서입니다:

1. **맵 선언 및 초기화**

```Go
package main

import "fmt"

func main() {
    // 문자열 키와 정수 값이 있는 빈 맵 초기화
    var scores map[string]int
    fmt.Println(scores) // 출력: map[]

    // 비어있지 않은 맵 선언 및 초기화
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // 출력: map[green:#00ff00 red:#ff0000]
}
```

2. **요소 추가 및 접근하기**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // 출력: 5
}
```

3. **맵 순회하기**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // 출력 순서는 달라질 수 있음, 맵은 순서를 보장하지 않음.
}
```

4. **요소 삭제하기**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // 삭제 전

    delete(meals, "lunch")
    fmt.Println(meals) // 삭제 후
}
```

## 심층 분석

Go 1에서 소개된 맵은 연관 배열을 효율적으로 다룰 수 있는 내장 방법을 제공합니다. 슬라이스가 순서가 있는 컬렉션이라는 점과 달리, 맵은 순서가 없습니다. 이는 맵 요소에 대한 반복 순서가 실행마다 동일하게 보장되지 않음을 의미합니다. 이는 동적으로 키-값 쌍을 처리할 수 있는 능력과 상당한 유연성을 위한 타협점입니다.

내부적으로 Go는 맵을 해시 테이블로 구현하여 접근, 삽입, 삭제 작업의 평균 복잡도가 대부분의 상황에서 O(1)이 되도록 보장합니다. 하지만, 해시 충돌과 같은 요소에 따라 이 효율성이 변할 수 있다는 점을 주목할 필요가 있습니다.

키 순회가 필요한 사용 사례의 경우, 맵과 슬라이스를 결합하거나 정렬된 맵이나 트리와 같은 추가적인 데이터 구조를 제공하는 서드 파티 패키지를 탐색할 수도 있습니다. 그럼에도 불구하고 Go의 맵은 많은 프로그래밍 시나리오에서 강력하고 필수적인 도구입니다.
