---
title:                "정규 표현식 사용하기"
aliases:
- /ko/swift/using-regular-expressions/
date:                  2024-02-03T19:18:40.671803-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
정규 표현식, 또는 regex는 검색 패턴을 형성하는 문자의 연속이며, 종종 문자열 일치 또는 조작 작업에 사용됩니다. 프로그래머들은 데이터 유효성 검사 및 파싱부터 변환에 이르기까지 모든 것에 이를 활용하여, 다양한 프로그래밍 언어에서 텍스트 처리 및 조작 작업에 있어 필수 도구로 만듭니다. 이에는 Swift도 포함됩니다.

## 방법:
Swift에서 정규 표현식을 지원하는 기본적인 방법은 `NSRegularExpression` 클래스와 String 클래스의 범위 및 교체 메소드를 함께 사용하는 것입니다. 아래는 텍스트 블록 내에서 이메일 주소를 찾아 하이라이트 하는 정규 표현식 사용 예입니다:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Found: \(text[range])")
        }
    } else {
        print("No matches found.")
    }
} catch {
    print("Regex error: \(error.localizedDescription)")
}

// 샘플 출력:
// Found: support@example.com
// Found: feedback@example.org
```

보다 복잡하거나 편리성 중심의 시나리오를 위해서, SwiftRegex와 같은 제3의 라이브러리를 사용할 수 있으며, 이는 문법을 단순화하고 가능성을 확장합니다. Swift의 표준 라이브러리는 강력하지만, 일부 개발자들은 이러한 라이브러리들을 그들의 간결한 문법과 추가 기능 때문에 선호합니다. 다음은 가상의 제3의 라이브러리를 사용하여 유사한 작업을 수행하는 방법입니다:

```swift
// SwiftRegex라는 라이브러리가 존재하고 임포트된다고 가정
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // SwiftRegex에 의해 제공되는 가상의 메소드
if emails.isEmpty {
    print("No email addresses found.")
} else {
    emails.forEach { email in
        print("Found: \(email)")
    }
}

// SwiftRegex의 `matches(for:)` 메소드가 존재한다고 가정할 때의 가상의 출력:
// Found: hello@world.com
```

이 예는 문자열 내에서 일치하는 항목을 찾기 위해 제3의 정규 표현식 패키지를 사용하여 단순화하는 방법을 보여줍니다. 여기서 `matches(for:)`와 같은 편리 메소드가 존재한다고 가정합니다. 정확한 문법과 메소드 사용 가능 여부를 위해서는 각 제3의 라이브러리 문서를 참조하는 것이 중요합니다.
