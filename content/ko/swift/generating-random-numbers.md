---
title:                "난수 생성하기"
date:                  2024-01-20T17:50:19.699085-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

랜덤 숫자 생성은 예측할 수 없는 수를 만드는 것입니다. 프로그래머들은 게임, 보안, 데이터 분석 등에서 요소의 불확실성을 더하기 위해 사용합니다.

## How to: (방법)

Swift에서 랜덤 수를 생성하는 것은 직관적입니다. 여기 간단한 예제가 있습니다:

```swift
import Foundation

// 1부터 100 사이의 랜덤 정수 생성
let randomInt = Int.random(in: 1...100)
print(randomInt)

// 0.0에서 1.0 사이의 랜덤 Double 생성
let randomDouble = Double.random(in: 0...1)
print(randomDouble)

// 배열에서 랜덤 요소 선택
let colors = ["red", "green", "blue", "yellow"]
if let randomColor = colors.randomElement() {
    print(randomColor)
}

// 배열을 랜덤으로 섞기
let shuffledColors = colors.shuffled()
print(shuffledColors)
```

이 코드를 실행하면, 다음과 같은 출력을 볼 수 있습니다 (매번 다를 것입니다):

```
42
0.8431554
green
["yellow", "red", "blue", "green"]
```

## Deep Dive (심층 분석)

Swift의 랜덤 수 생성은 애플의 GameplayKit 프레임워크가 있기 전부터 있었습니다. GameplayKit는 다양한 랜덤화 기능을 제공하지만, Swift의 표준 라이브러리는 단순성을 위해 설계되었습니다. `random(in:)` 메소드는 내부적으로 장치의 엔트로피 소스를 사용하여 랜덤 수를 생성합니다.

다음은 몇 가지 대안입니다:
- `arc4random_uniform()`: 이전 버전의 Swift나 Objective-C에서 널리 사용되었습니다.
- `/dev/random`, `/dev/urandom`: 유닉스 계열 시스템에서 사용하는 난수 파일입니다.

랜덤 수 생성이 중요한 보안 기능을 하는 경우, 예측할 수 없는 충분한 엔트로피가 확보되어야 합니다.

## See Also (추가 정보)

- Swift 공식 문서: https://developer.apple.com/documentation/swift
- GameplayKit 문서: https://developer.apple.com/documentation/gameplaykit
- Swift의 랜덤 API 개선에 대한 Swift 포럼 토론: https://forums.swift.org/t/se-0202-random-unification/11313
