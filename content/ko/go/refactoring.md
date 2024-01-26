---
title:                "리팩터링"
date:                  2024-01-26T01:37:25.472996-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩터링"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/refactoring.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩토링은 기존 컴퓨터 코드의 구조를 외부 동작을 변경하지 않고 재구성하는 과정입니다. 프로그래머들은 소프트웨어의 비기능적 속성(가독성 및 유지 보수성 등)을 향상시켜 코드 이해를 용이하게 하고, 복잡성을 줄이며, 버그를 더 쉽게 찾을 수 있도록 하기 위해 이 작업을 수행합니다.

## 방법:
간단한 Go 코드 리팩토링 예제로 깊이 들어가 보겠습니다. 숫자 슬라이스의 평균을 계산하는 코드 조각을 가져와 명확성과 재사용성을 위해 리팩토링하겠습니다.

원본 코드:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

리팩토링된 코드:
```Go
package main

import "fmt"

// CalculateAverage는 float64의 슬라이스를 받아 평균을 반환합니다.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

리팩토링된 코드에서는 평균을 계산하는 로직을 `CalculateAverage`라는 별도의 함수로 추출했습니다. 이것은 `main` 함수를 더 간결하게 만들고, 평균 계산 로직을 재사용 가능하고 테스트하기 쉽게 합니다.

## 심층 분석
코드 리팩토링은 현대의 개념이 아니며, 널리 사용되기 전의 컴퓨터에서도 시작되었습니다. 그 실천은 기계 공학 또는 그 이전의 영역에서 시작되었을 가능성이 있습니다. 소프트웨어에서는 1990년대 객체 지향 프로그래밍 및 익스트림 프로그래밍(XP)의 등장과 함께 더 공식화되었으며, 마틴 파울러의 주요한 책 "리팩토링: 기존 코드의 설계 개선"에 의해 주요하게 영향을 받았습니다.

리팩토링 기술에는 변수 명명 변경으로부터 메소드나 클래스 추출과 같이 더 복잡한 패턴까지 다양합니다. 중요한 것은 소프트웨어의 기능을 변경하지 않고 내부 구조를 개선하는 작은 증분 변경을 하는 것입니다.

Go를 사용할 때, 언어의 단순성과 강력한 표준 라이브러리로 인해 리팩토링이 간단해질 수 있습니다. 하지만, 리팩토링이 버그를 도입하지 않도록 좋은 단위 테스트 세트를 갖추는 것이 여전히 중요합니다. `gorename` 및 `gofmt`과 같은 도구들은 일부 프로세스를 자동화하는 데 도움을 주고, IDE는 종종 내장 리팩토링 지원을 가지고 있습니다.

수동 리팩토링 외에도 Go에는 GoLand의 리팩토링 도구와 Go Refactor와 같은 몇 가지 자동 코드 리팩토링 도구가 있습니다. 이 도구들은 프로세스를 가속화할 수 있지만, 코드를 이해하고 고려된 변경을 하는 것에 대한 대체물은 아닙니다.

## 참고 자료
 - [Go에서의 리팩토링: 단순함이 아름답다](https://go.dev/blog/slices)
 - [효과적인 Go: 인터페이스를 사용한 리팩토링](https://go.dev/doc/effective_go#interfaces)
 - [마틴 파울러의 리팩토링 페이지](https://refactoring.com/)
 - [GoLand 리팩토링 도구](https://www.jetbrains.com/go/features/refactorings/)