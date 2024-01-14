---
title:    "Swift: 정규 표현식 사용하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜
정규 표현식을 사용하는 이유는 문자열을 검색, 추출 및 대체하는 데 유용하기 때문입니다.

## 사용 방법
정규 표현식은 문자열을 비교하는 강력한 도구입니다. Swift에서 정규 표현식을 사용하려면 다음 단계를 따르세요.

```Swift
import Foundation

let str = "안녕하세요, 저는 Swift를 공부하고 있습니다!"

// "안녕"이 들어간 부분 찾기
let pattern = "안녕"

// 정규식 객체 생성
let regex = try! NSRegularExpression(pattern: pattern, options: [])

// 문자열에서 검색
let matches = regex.matches(in: str, options: [], range: NSRange(location: 0, length: str.utf16.count))

// 찾은 부분 출력
for match in matches {
    let range = match.range
    let startIndex = str.index(str.startIndex, offsetBy: range.location)
    let endIndex = str.index(str.startIndex, offsetBy: range.location + range.length)
    let matchString = String(str[startIndex..<endIndex])
    print(matchString)
}

// Output: "안녕하세요"
```

## 깊이 파고들기
정규 표현식은 문자열에 더욱 복잡한 패턴을 적용할 수 있는 많은 옵션을 제공합니다. 예를 들어, 다양한 패턴 매칭 옵션으로 대소문자를 구분할 수 있고, 부분적일치나 전체적일치를 선택할 수 있습니다. 정규 표현식 패턴을 더 많이 배우려면 직접 실험해보고 다른 사용 사례를 찾아보세요.

## 더 알아보기
[정규 표현식 소개](https://www.youtube.com/watch?v=rhzKDrUiJVk&t=301s)  
[Swift에서 정규 표현식 사용하기](https://medium.com/@abhimuralidharan/regular-expression-in-swift-7ed46aaa6b6c)  
[Swift의 Foundation 프레임워크: 정규 표현식](https://mobikul.com/regular-expression-swift/)