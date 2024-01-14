---
title:                "Swift: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜: 패턴과 일치하는 문자를 삭제하는 것에 참여하는 이유를 설명하는 1-2 문장.

문자를 삭제 한다는 것은 많은 이유로 인해 유용 할 수 있습니다. 예를 들어, 문자열에서 특정 단어를 제거하거나, 사용자가 실수로 입력한 오탈자를 수정하는 등의 작업을 할 수 있습니다.

## 방법: " ```Swift ... ```" 코드 블록 내부에 코딩 예제와 샘플 출력.

패턴을 사용하여 문자를 삭제하는 방법은 간단합니다. 우선, 삭제할 문자열을 정의한 다음, 문자열에서 일치하는 패턴을 찾고 삭제하는 메소드를 사용하면 됩니다. 아래 예제 코드를 참고하세요.

```
// 삭제할 문자열을 정의합니다.
let str = "Hello World!"

// 문자열에서 "l"이 포함된 모든 문자를 삭제합니다.
let newStr = str.replacingOccurrences(of: "l", with: "")

print(newStr) // "Heo Word!"
```

## 딥 다이브: 패턴과 일치하는 문자를 삭제하는 더 깊은 정보.

위의 예제에서 사용된 `replacingOccurrences(of:with:)`는 `String` 클래스의 메소드입니다. 이 메소드는 문자열에서 일치하는 패턴을 찾고, 해당 패턴을 주어진 다른 문자열로 대체합니다. 이 외에도 `removingAll(where:)` 메소드를 사용하여 조건을 만족하는 모든 문자를 삭제할 수 있습니다.

```
let str = "Hello Swift!"

// "a" 또는 "s"를 포함하는 모든 문자를 삭제합니다.
let newStr = str.removingAll { $0 == "a" || $0 == "s" }

print(newStr) // "Hello wtift!"
```

## 참고: "See Also"라는 Markdown 제목과 링크 목록으로 끝납니다.

- [String 클래스 리퍼런스](https://developer.apple.com/documentation/swift/string)
- [String 메소드 사용 예제](https://www.dotnetperls.com/string-swift)
- [Swift 문자열 조작 가이드](https://www.hackingwithswift.com/articles/138/swift-string-manipulation-tips-and-tricks)