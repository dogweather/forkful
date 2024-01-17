---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Swift: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
패턴과 일치하는 문자를 삭제하는 것은 프로그래머들이 특정 문자를 제거하고자 할 때 사용하는 방법입니다.

## 어떻게:
```Swift
let string = "Hello Swift!"
let modified = string.replacingOccurrences(of: "l", with: "")
print(modified)
```
```
Heo Swift!
```

```Swift
let array = ["apple", "banana", "cherry"]
let modified = array.filter { $0 != "banana" }
print(modified)
```
```
["apple", "cherry"]
```

## 깊이 파고들기:
(1) 이 기능의 역사적 배경 (2) 대안들 (3) 패턴과 일치하는 문자를 삭제하는 방법에 대한 구체적인 내용

패턴과 일치하는 문자를 삭제하는 기능은 주로 문자열이나 배열에서 부분적으로 제거하고자 할 때 사용됩니다. 예를 들어, 특정 문자를 포함하지 않는 새로운 문자열을 만들거나 특정 항목을 제거한 새로운 배열을 생성하고 싶을 때 자주 사용됩니다.

```Swift
let string = "Hello World!"
let modified = string.replacingOccurrences(of: "o", with: "")
print(modified)
```
```
Hell Wrld!
```

대안으로는 ```filter``` 메서드를 사용하는 방법이 있습니다. 이 메서드는 배열에서 특정 조건을 만족하는 항목만을 남길 수 있도록 해줍니다. 위의 예시에서는 "banana"를 제외한 모든 항목을 남기는 방식으로 사용하였습니다.

이 기능은 다양한 데이터 타입에서도 사용할 수 있습니다. 딕셔너리에서 값이나 키를 삭제하거나, 정규표현식을 사용하여 패턴과 일치하는 문자를 삭제하는 등의 다양한 방법으로 활용할 수 있습니다.

## 관련 정보:
- [Apple 공식 문서](https://developer.apple.com/documentation/foundation/nsstring/1412491-replacingoccurrences)
- [Swift Standard Library 문서](https://developer.apple.com/documentation/swift/substring/1643114-replacingoccurrences)
- [Swift by Sundell: 문맥 기반 문자열 검색](https://www.swiftbysundell.com/tips/contextual-string-searching/)