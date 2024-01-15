---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Swift: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

패턴과 일치하는 문자를 삭제하는 것이 유용한 이유는 데이터 정제와 특정 문자를 필터링할 때 유용하고 코드를 간결하게 유지하는 데 도움이 될 수 있기 때문입니다.

## 방법

```Swift
// 다음과 같은 문자열이 있다고 가정해봅시다.
let str = "a1b2c3d4e5"

// 패턴과 일치하는 문자를 삭제하기위해 NSRegularExpression을 사용합니다.
let pattern = "[0-9]"
let regex = try! NSRegularExpression(pattern: pattern, options: .caseInsensitive)
let result = regex.stringByReplacingMatches(in: str, options: [], range: NSMakeRange(0, str.length), withTemplate: "")

print(result)
// 출력 결과: abcde
```

## 깊은 곳을 들어가보기

해당 패턴과 일치하는 문자를 삭제하는 데 사용되는 NSRegularExpression은 사용하기 전에 많은 작업을 거친다는 것을 알아야합니다. 옵션과 범위를 설정하는 방법과 함께 NSRegularExpression을 사용하는 것이 코드의 효율성을 높이는 데 도움이 될 수 있습니다.

## 참고

- [NSRegularExpression 공식 문서](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift 문자열 다루기](https://developer.apple.com/library/archive/documentation/StringsTextFonts/Conceptual/TextAndWebiPhoneOS/WorkingwithStrings/WorkingwithStrings.html#//apple_ref/doc/uid/TP40009542-CH3-51633)