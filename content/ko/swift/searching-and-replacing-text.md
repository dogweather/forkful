---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 특정 패턴이나 문자열을 찾아 다른 문자열로 교체하는 프로그래밍 기법입니다. 이 작업은 데이터 정리, 데이터 변환, 코드 리팩토링 등 다양한 상황에서 필수적입니다.

## 어떻게 할까?

Swift에는 String 클래스에서 제공하는 `replacingOccurrences(of:with:)` 메서드를 통해 특정 문자열을 탐색하고 교체할 수 있습니다.

```Swift
var message = "Hello, world!"
message = message.replacingOccurrences(of: "world", with: "Swift")
print(message)
```

이 코드를 실행하면, 'world'가 'Swift'으로 변경된 'Hello, Swift!'를 출력합니다.

## 깊게 알아보기

텍스트 검색 및 교체는 프로그래밍의 역사만큼 오래되었습니다. 첫 컴퓨터의 시대부터 데이터를 수정하고 기존 패턴을 새롭게 고치기 위한 필수 도구였습니다. Swift 외에도 Java, Python, C++ 등 많은 언어에서 이 기능을 지원합니다.

Swift에서는 `replacingOccurrences(of:with:)` 메서드를 통해 이 작업을 수행합니다. 내부적으로는 문자열을 탐색하고 일치하는 부분을 찾아 이를 새 문자열로 교체하는 과정을 거칩니다.

또한 Swift에서는 정규식을 사용하여 텍스트 검색 및 교체를 할 수도 있습니다. 정규식을 사용하면 더욱 복잡한 패턴 매칭과 교체 작업을 수행할 수 있습니다.

## 참고문헌

- 문자열 처리에 대한 일반적인 가이드 ["Working with Strings in Swift"](https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-more-developers-should-be-using)