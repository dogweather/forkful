---
title:                "정규식을 사용하는 방법"
html_title:           "Swift: 정규식을 사용하는 방법"
simple_title:         "정규식을 사용하는 방법"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

정규 표현식을 사용하는 것은 문자열에서 특정 패턴을 찾거나 매치하는 것을 가능하게 해주는 프로그래밍 기술입니다. 프로그래머들은 정규 표현식을 사용하여 복잡한 텍스트 처리 작업을 간단하고 빠르게 처리할 수 있습니다.

## 방법:

정규 표현식을 사용하기 위해서는 Swift에서 제공하는 ```NSRegularExpression``` 클래스를 사용해야 합니다. 이 클래스는 패턴과 매치할 문자열을 입력받아 원하는 결과를 도출합니다. 예를 들면:

```Swift
let text = "Hello, World!"
let pattern = "\\w+"
let regex = try? NSRegularExpression(pattern: pattern, options: .caseInsensitive)
let matches = regex?.matches(in: text, options: [], range: NSRange(location: 0, length: text.count))
matches?.forEach { match in
    let matchText = text[Range(match.range, in: text)!]
    print("매치된 문자열: \(matchText)")
}
```
위의 코드는 문자열에서 알파벳 문자열만 매치하는 예제입니다. 결과는 다음과 같이 출력됩니다:

```
매치된 문자열: Hello
매치된 문자열: World
```

## 깊이 파고들기:

정규 표현식은 1950년대에 고안된 존 그래워트에 의해 개발되었습니다. 그리고 이후로 계속해서 발전하고 있습니다. 현재는 수많은 언어에서 지원됩니다. 다른 대안으로는 문자열에서 패턴을 매치하는 데 사용하는 정규 표현식 대신 문자열 메소드를 사용할 수도 있습니다. 하지만, 정규 표현식은 적절한 패턴을 사용하면 더 간단하고 빠르게 작업할 수 있습니다. 정규 표현식을 사용할 때, 조금 더 복잡한 수준의 패턴을 이해하고 작성하는 데 시간이 걸릴 수 있지만, 익숙해지면 효율적인 코드 작성에 큰 도움이 됩니다.

## 참고 자료:

- [Swift 공식 문서](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [정규 표현식 테스트 사이트](https://regexr.com/)