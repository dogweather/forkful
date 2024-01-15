---
title:                "랜덤 숫자 생성하기"
html_title:           "Swift: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?

랜덤한 숫자를 생성하는 데 참여하려는 이유 중 하나는 게임이나 시뮬레이션 등의 프로그램에서 다양한 결과를 얻고자 하는 것일 수 있습니다.

## 하는 방법

랜덤한 숫자를 생성하는 방법에 대한 몇 가지 예제를 살펴보겠습니다.

```Swift
// 1에서 10 사이의 랜덤한 정수 생성
let randomInt = Int.random(in: 1...10)
print(randomInt) // 예상 출력: 5

// 0에서 1 사이의 랜덤한 실수 생성
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble) // 예상 출력: 0.587743148583387
```

이 외에도 `Int.random(in: Range)`와 `Double.random(in: Range)` 같은 다양한 메소드를 사용해 원하는 범위 내에서 랜덤한 숫자를 생성할 수 있습니다.

## 깊이 파헤치기

랜덤한 숫자를 생성하는 기술로는 난수 발생기(pseudo-random number generator, PRNG)를 사용합니다. PRNG는 난수 시퀀스를 생성하기 위해 초기 값(seed)에 따라 동일한 순서로 숫자를 생성하며, 이를 통해 예측 불가능한 결과를 얻을 수 있습니다. 하지만 PRNG는 사실 진짜 랜덤한 숫자가 아니므로 보안적으로 안전하지 않은 용도에는 사용하지 않도록 주의해야 합니다.

## 참고 자료

- [Swift 공식 문서 - Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID310)
- [Swift 4.2: 난수 생성하기](https://zeddios.tistory.com/392)
- [난수 발생기와 난수 분포](https://ko.wikipedia.org/wiki/%EB%82%9C%EC%88%98_%EB%B0%9C%EC%83%9D%EA%B8%B0%EC%99%80_%EB%82%9C%EC%88%98_%EB%B6%84%ED%8F%AC)