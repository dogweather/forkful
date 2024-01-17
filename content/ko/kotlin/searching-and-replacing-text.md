---
title:                "텍스트 검색 및 교체"
html_title:           "Kotlin: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
검색과 대체 텍스트는 프로그래머들이 코드에서 특정 부분을 찾아서 다른 내용으로 변경하는 작업을 말합니다. 이 작업을 하는 이유는 코드를 더 효율적으로 만들기 위해서입니다.

## 방법:
### 검색:
```Kotlin
val text = "Hello World"
val searchedText = text.replace("Hello", "Hola")
println(searchedText)
```
#### 출력 결과:
`Hola World`

### 대체:
```Kotlin
val text = "My name is John"
val replacedText = text.replace("John", "Jane")
println(replacedText)
```
#### 출력 결과:
`My name is Jane`

## 깊게 들어가기:
### 역사적 배경:
텍스트 검색 및 대체는 이전부터 프로그래밍에서 중요한 작업으로 여겨졌습니다. 이전에는 일일이 수작업으로 찾아서 변경하는 방식을 사용했지만, 현재는 컴퓨터 프로그램을 사용해 더 효율적으로 처리할 수 있습니다.

### 대안:
검색과 대체 작업은 프로그래밍에서 매우 중요하지만, 이 외에도 다양한 작업을 할 수 있는 다른 방법들이 존재합니다. 예를 들어, 정규표현식을 사용하면 더 복잡한 검색과 대체 작업을 수행할 수 있습니다.

### 구현 세부 사항:
Kotlin에서는 `replace()` 메서드를 사용해서 검색과 대체 작업을 수행할 수 있습니다. 이 메서드는 첫 번째 인자로 검색할 내용, 두 번째 인자로 대체할 내용을 받습니다. 또한 이 메서드는 기존 문자열을 변경하는 것이 아니라 새로운 문자열을 반환하기 때문에 원래 문자열은 그대로 유지됩니다.

## 관련 정보:
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#strings)