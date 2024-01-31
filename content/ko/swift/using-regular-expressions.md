---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾고 조작하는 표준화된 방법입니다. 프로그래머들은 복잡한 검색과 데이터 가공을 효율적으로 처리하기 위해 이를 사용합니다.

## How to: (방법)
```Swift
import Foundation

let sampleText = "Swift는 2023년에 더욱 멋진 기능들이 추가되었습니다."
let pattern = "\\d{4}"

if let regex = try? NSRegularExpression(pattern: pattern) {
    let matches = regex.matches(in: sampleText, range: NSRange(sampleText.startIndex..., in: sampleText))
    
    let years = matches.map {
{
        String(sampleText[Range($0.range, in: sampleText)!])
    }
    print(years) // ["2023"]
} else {
    print("정규 표현식을 생성하지 못했습니다.")
}
```

## Deep Dive (깊이 있게 탐구)
### 역사적 맥락
1960년대, 켄 톰프슨이 유닉스 내에서 텍스트 처리를 위해 정규 표현식을 고안했습니다. 이후 다양한 언어와 시스템에서 기본적인 처리 기법으로 자리잡았습니다.

### 대안들
정규 표현식 대신 문자열 처리 메서드(string methods)나 파서(parser) 라이브러리를 사용할 수 있지만, 간단한 작업에는 더 복잡하고 느릴 수 있습니다.

### 구현 세부사항
Swift에서는 `NSRegularExpression` 클래스를 사용하여 정규 표현식 기능을 적용합니다. 이를 사용하여 패턴 매칭, 데이터 추출, 문자열 치환 등을 수행할 수 있습니다.

## See Also (참조)
- [Swift 문자열과 문자 안내서](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [NSRegularExpression Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
