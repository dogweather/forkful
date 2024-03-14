---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:09.893340-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB450 \uAC1C \uC774\uC0C1\uC758\
  \ \uBB38\uC790\uC5F4\uC744 \uB05D\uACFC \uB05D\uC744 \uC774\uC5B4 \uD558\uB098\uC758\
  \ \uC0C8\uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uD615\uC131\uD558\uB294 \uACFC\uC815\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB3D9\
  \uC801\uC73C\uB85C \uD14D\uC2A4\uD2B8\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574\
  , \uC608\uB97C \uB4E4\uC5B4 \uBA54\uC2DC\uC9C0, \uACBD\uB85C, \uB610\uB294 \uBCF5\
  \uC7A1\uD55C \uCFFC\uB9AC\uB97C \uAD6C\uC131\uD558\uAE30 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294 \uD504\uB85C\uADF8\uB7A8\
  \uC744 \uB354 \uC0C1\uD638\uC791\uC6A9\uC801\uC774\uACE0\u2026"
lastmod: '2024-03-13T22:44:54.442045-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB450 \uAC1C \uC774\uC0C1\uC758 \uBB38\
  \uC790\uC5F4\uC744 \uB05D\uACFC \uB05D\uC744 \uC774\uC5B4 \uD558\uB098\uC758 \uC0C8\
  \uB85C\uC6B4 \uBB38\uC790\uC5F4\uC744 \uD615\uC131\uD558\uB294 \uACFC\uC815\uC744\
  \ \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB3D9\uC801\
  \uC73C\uB85C \uD14D\uC2A4\uD2B8\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574, \uC608\
  \uB97C \uB4E4\uC5B4 \uBA54\uC2DC\uC9C0, \uACBD\uB85C, \uB610\uB294 \uBCF5\uC7A1\uD55C\
  \ \uCFFC\uB9AC\uB97C \uAD6C\uC131\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \ \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294 \uD504\uB85C\uADF8\uB7A8\uC744 \uB354\
  \ \uC0C1\uD638\uC791\uC6A9\uC801\uC774\uACE0\u2026"
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결은 두 개 이상의 문자열을 끝과 끝을 이어 하나의 새로운 문자열을 형성하는 과정을 말합니다. 프로그래머들은 동적으로 텍스트를 생성하기 위해, 예를 들어 메시지, 경로, 또는 복잡한 쿼리를 구성하기 위해 이 작업을 수행합니다. 이는 프로그램을 더 상호작용적이고 반응적으로 만듭니다.

## 방법:

Go에서는 문자열을 연결하는 몇 가지 방법이 있습니다. 여기 몇 가지 일반적인 방법들과 예시를 살펴보겠습니다:

### `+` 연산자 사용하기:
문자열을 연결하는 가장 간단한 방법은 `+` 연산자를 사용하는 것입니다. 여러 문자열에 대해선 가장 효율적이지 않지만 직관적입니다.
```go
firstName := "John"
lastName := "Doe"
fullName := firstName + " " + lastName
fmt.Println(fullName) // John Doe
```

### `fmt.Sprintf` 사용하기:
변수가 있는 문자열을 포맷팅하기 위해, `fmt.Sprintf`는 매우 유용합니다. 출력 형식을 더 많이 제어할 수 있습니다.
```go
age := 30
message := fmt.Sprintf("%s는 %d살입니다.", fullName, age)
fmt.Println(message) // John Doe는 30살입니다.
```

### `strings.Builder` 사용하기:
특히 반복문에서 여러 문자열을 연결할 때, `strings.Builder`는 효율적이고 권장됩니다.
```go
var builder strings.Builder
words := []string{"hello", "world", "from", "go"}

for _, word := range words {
    builder.WriteString(word)
    builder.WriteString(" ")
}

result := builder.String()
fmt.Println(result) // hello world from go 
```

### `strings.Join` 사용하기:
구체적인 구분자로 문자열 슬라이스를 연결해야 할 때, `strings.Join`이 최적의 선택입니다.
```go
elements := []string{"path", "to", "file"}
path := strings.Join(elements, "/")
fmt.Println(path) // path/to/file
```

## 심층 탐구

문자열 연결은 Go에서 문자열을 처리하는 방식에 대해 더 깊은 측면에 관한 것처럼 보일 수 있습니다. Go에서 문자열은 불변입니다. 즉, 모든 연결 작업은 새 문자열을 생성합니다. 이는 큰 수의 문자열을 연결하거나, 빠른 반복문에서 수행할 때, 빈번한 메모리 할당 및 복사로 인해 성능 문제를 초래할 수 있습니다.

역사적으로, 언어들은 문자열의 불변성과 연결 효율성을 다양한 방식으로 다루어 왔고, Go의 `strings.Builder` 및 `strings.Join`을 통한 접근 방식은 프로그래머들에게 사용 용이성과 성능 사이의 균형을 제공하는 도구를 제공합니다. 특히 Go 1.10에서 도입된 `strings.Builder` 타입은 여러 번의 문자열 할당 부담 없이 문자열을 구축할 수 있는 효율적인 방법을 제공합니다. 이는 필요에 따라 커지는 버퍼를 할당하고, 그 안에 문자열이 추가됩니다.

이러한 옵션에도 불구하고, 컨텍스트에 기반한 올바른 방법을 선택하는 것이 중요합니다. 간단하거나 드물게 연결되는 경우, 단순 연산자 또는 `fmt.Sprintf`가 충분할 수 있습니다. 그러나 성능이 중요한 경로에서, 특히 여러 번의 연결이 관련된 경우, `strings.Builder` 또는 `strings.Join`을 활용하는 것이 더 적절할 수 있습니다.

Go가 문자열 조작을 위한 강력한 내장 기능을 제공하긴 하지만, 이러한 기능의 기본적인 성능 특성을 인지하는 것이 중요합니다. `+` 또는 `fmt.Sprintf`을 통한 연결 방법은 간단함과 소규모 연산에 잘 맞지만, Go의 효율적인 문자열 구축 관행을 이해하고 활용하는 것은 애플리케이션이 성능이 좋고 확장 가능하게 유지되도록 보장합니다.
