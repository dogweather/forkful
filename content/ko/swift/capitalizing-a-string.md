---
title:                "문자열 대문자화"
html_title:           "Swift: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 대문자 변환은 소문자 문자를 대문자로 변환하는 것을 의미합니다. 프로그래머들은 규현, 사용자 입력 정규화, 출력 포맷팅 등의 이유로 이 작업을 수행합니다.

## 방법:

Swift에서는 `.uppercased()` 함수를 사용하여 문자열을 대문자로 바꿀 수 있습니다. 간단히 사용하기에 아주 적합한 함수입니다.

```swift
let example = "hello world"
let uppercased = example.uppercased()

print(uppercased) // Output: HELLO WORLD
```
위의 예제 코드에서, `uppercased()` 함수는 원본 문자열인 "hello world"를 "HELLO WORLD"로 변환합니다.

## 디피 다이브:

문자열의 대문자 변환은 오래전부터 있던 기능입니다. 예를 들어, 기존의 Fortran 언어에서는 `ACHAR` 함수를 사용하여 이 작업을 수행했습니다.

대문자 변환에 대한 대안으로는 문자열을 반복하면서 각 문자를 개별적으로 대문자로 변환하는 방법이 있습니다. 다만, Swift에서는 `.uppercased()` 함수를 제공하여 이런 작업을 더욱 쉽게 할 수 있게 하였습니다.

Swift의 `.uppercased()` 함수는 문자열을 통해 호출되며, 모든 문자를 대문자로 변환하여 새로운 문자열을 반환합니다. 이 함수는 원래 문자열을 변경하지 않으므로, Swift의 문자열 불변성을 유지합니다.

## 추천도서:

1. "The Swift Programming Language" Apple의 공식 소개서에서 Swift에 대해 더욱 자세히 배울 수 있습니다. [링크](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)


3. "Pro Swift"는 Swift를 사용하여 실제 프로젝트를 어떻게 수행할 수 있는지에 대해 설명합니다. [링크](https://www.hackingwithswift.com/store/pro-swift)