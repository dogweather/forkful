---
title:                "Gleam: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 – Gleam에서 텍스트 검색 및 교체하는 방법

## 왜?

텍스트 검색과 교체는 매우 일반적이고 기본적인 프로그래밍 기술입니다. 예를 들어, 당신이 특정 단어 또는 구문을 찾아서 바꾸어야 한다면, 일일이 모든 단어를 찾고 바꾸는 것은 매우 번거롭고 오류를 발생시킬 수 있습니다. Gleam은 간단하고 효율적으로 텍스트를 검색하고 교체하는 기능을 제공하여 코드 작성을 더 쉽고 효율적으로 만들어줍니다.

## 사용 방법

Gleam에서 텍스트 검색과 교체는 매우 간단합니다. 아래의 예제 코드를 참조하여 실제로 어떻게 동작하는지 확인해보세요:

```Gleam
// 문자열에서 "Hello"를 "안녕하세요"로 변경하기
let message = "Hello there!"
let updated_message = message.replace("Hello", "안녕하세요")
// updated_message: "안녕하세요 there!"

// 리스트에서 특정 요소를 검색하고 제거하기
let fruits = ["apple", "banana", "orange"]
let updated_fruits = fruits.replace("banana", "")
// updated_fruits: ["apple", "orange"]
```

위 예제에서 볼 수 있듯이, `.replace(검색어, 교체할_값)` 함수를 사용하여 텍스트를 검색하고 교체할 수 있습니다. 또한 문자열 뿐만 아니라 리스트에서도 동일한 방식으로 사용할 수 있습니다.

## 깊이 있는 탐구

Gleam에서 텍스트 검색과 교체는 어떻게 동작할까요? 이를 이해하기 위해서는 Gleam에서 문자열을 다루는 방식을 알아야 합니다. Gleam에서 문자열은 실제로는 단순한 byte 배열로 표현됩니다. 문자열 내부에서 검색 및 교체가 발생하면, Gleam은 바이트 배열을 조작하여 검색어를 찾고 교체할 값을 삽입합니다. 이러한 과정은 매우 빠르고 효율적으로 동작하도록 최적화되어 있습니다.

또한 Gleam에서는 정규식을 사용하여 텍스트를 검색하고 교체하는 기능도 제공합니다. 정규식을 사용하면 더욱 정교한 검색 및 교체를 할 수 있습니다.

## 참고 자료

- [Gleam 공식 문서 – 문자열 함수](https://gleam.run/documentation/std_lib/string.html)
- [Gleam 공식 문서 – 정규식 함수](https://gleam.run/documentation/std_lib/regex.html)