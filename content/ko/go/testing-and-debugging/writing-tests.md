---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:14.873761-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C\uB294 \uC77C\uBC18\uC801\uC73C\uB85C \uD14C\
  \uC2A4\uD2B8 \uB300\uC0C1 \uCF54\uB4DC\uC640 \uAC19\uC740 \uD328\uD0A4\uC9C0\uC5D0\
  \ \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD569\uB2C8\uB2E4. \uD14C\uC2A4\uD2B8\uB97C\
  \ \uD3EC\uD568\uD558\uB294 \uD30C\uC77C\uC740 `_test.go` \uC811\uBBF8\uC0AC\uB85C\
  \ \uBA85\uBA85\uB429\uB2C8\uB2E4. \uD14C\uC2A4\uD2B8\uB294 `testing` \uD328\uD0A4\
  \uC9C0\uC5D0\uC11C \uAC00\uC838\uC628 `testing.T` \uAC1D\uCCB4\uC5D0 \uB300\uD55C\
  \ \uD3EC\uC778\uD130\uB97C \uC778\uC218\uB85C \uBC1B\uB294 \uD568\uC218\uB4E4\uC774\
  \uBA70,\u2026"
lastmod: '2024-03-13T22:44:54.462100-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C\uB294 \uC77C\uBC18\uC801\uC73C\uB85C \uD14C\uC2A4\uD2B8 \uB300\
  \uC0C1 \uCF54\uB4DC\uC640 \uAC19\uC740 \uD328\uD0A4\uC9C0\uC5D0 \uD14C\uC2A4\uD2B8\
  \uB97C \uC791\uC131\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
Go에서는 일반적으로 테스트 대상 코드와 같은 패키지에 테스트를 작성합니다. 테스트를 포함하는 파일은 `_test.go` 접미사로 명명됩니다. 테스트는 `testing` 패키지에서 가져온 `testing.T` 객체에 대한 포인터를 인수로 받는 함수들이며, `t.Fail()`, `t.Errorf()` 등과 같은 메서드를 호출함으로써 실패를 신호합니다.

`math.go`에 정의된 함수 `Add`에 대한 간단한 테스트 예:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

테스트 파일 `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; 원하는 값 %d", result, expected)
    }
}
```

테스트 파일이 있는 동일한 디렉토리에서 `go test` 명령어로 테스트를 실행합니다. 테스트가 통과한 것을 나타내는 샘플 출력은 다음과 같습니다:

```
PASS
ok      example.com/my/math 0.002s
```

다양한 입력과 출력 조합을 효율적으로 테스트할 수 있는 테이블 주도 테스트(table-driven tests)의 경우, 테스트 케이스를 나타내는 구조체 슬라이스를 정의합니다:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("얻은 값 %d, 원하는 값 %d", ans, tt.expected)
            }
        })
    }
}
```

## 심층 분석
Go 1과 함께 소개된 Go 테스트 프레임워크는 Go 도구 체인과 완벽하게 통합되도록 설계되었으며, 이는 소프트웨어 개발에서 Go가 강조하는 단순성과 효율성을 반영합니다. 다른 언어의 일부 테스트 프레임워크가 외부 라이브러리나 복잡한 설정에 의존하는 반면, Go의 내장 `testing` 패키지는 테스트를 작성하고 실행하는 간단한 방법을 제공합니다.

Go의 테스트 접근 방식에서 흥미로운 측면은 파일 명명 패턴(`_test.go`)의 관례와 외부 의존성보다 표준 라이브러리 기능의 사용과 같은 관례 대 설정 원칙을 채택하는 것입니다. 이 최소주의 접근 방식은 진입 장벽이 낮기 때문에 개발자가 테스트를 작성하도록 장려합니다.

Go의 내장 테스트 기능이 많은 범위를 커버하지만, 모의 생성, 퍼즈 테스팅 또는 행동 주도 개발(BDD) 스타일 테스트와 같은 기능을 제공하는 제3자 도구나 프레임워크가 유용한 시나리오도 있습니다. Testify나 GoMock과 같은 인기 있는 라이브러리는 Go의 표준 테스팅 기능을 보완하여, 많은 의존성을 가진 복잡한 애플리케이션에서 특히 유용한 더 표현력 있는 단언이나 모의 생성 기능을 제공합니다.

이러한 대안이 존재함에도 불구하고, 표준 Go 테스팅 패키지는 단순성, 성능 및 언어 및 도구 체인과의 긴밀한 통합으로 인해 Go에서 테스팅의 핵심으로 남아 있습니다. 개발자가 제3자 도구로 보완을 선택하더라도, Go 테스팅 프레임워크는 코드 품질과 신뢰성을 보장하기 위한 견고한 기반을 제공합니다.
