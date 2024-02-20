---
date: 2024-01-26 01:10:51.942117-07:00
description: "\uD558\uC2A4\uCF08\uC5D0\uC11C \uCF54\uB4DC\uB97C \uD568\uC218\uB85C\
  \ \uAD6C\uC131\uD558\uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uC7AC\uC0AC\uC6A9 \uAC00\
  \uB2A5\uD558\uACE0 \uC774\uB984\uC774 \uC788\uB294 \uBE14\uB85D\uC73C\uB85C \uBD84\
  \uD574\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC65C \uADF8\uB7F4\
  \uAE4C\uC694? \uCF54\uB4DC\uB97C DRY(Do not Repeat Yourself, \uBC18\uBCF5\uD558\uC9C0\
  \ \uC54A\uAE30)\uD558\uAC8C \uC720\uC9C0\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uB192\
  \uC774\uBA70, \uB514\uBC84\uAE45\uC744 \uB354 \uC27D\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4\
  ."
lastmod: 2024-02-19 22:05:14.214752
model: gpt-4-1106-preview
summary: "\uD558\uC2A4\uCF08\uC5D0\uC11C \uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\
  \uC131\uD558\uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uC7AC\uC0AC\uC6A9 \uAC00\uB2A5\
  \uD558\uACE0 \uC774\uB984\uC774 \uC788\uB294 \uBE14\uB85D\uC73C\uB85C \uBD84\uD574\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC65C \uADF8\uB7F4\uAE4C\
  \uC694? \uCF54\uB4DC\uB97C DRY(Do not Repeat Yourself, \uBC18\uBCF5\uD558\uC9C0\
  \ \uC54A\uAE30)\uD558\uAC8C \uC720\uC9C0\uD558\uACE0, \uAC00\uB3C5\uC131\uC744 \uB192\
  \uC774\uBA70, \uB514\uBC84\uAE45\uC744 \uB354 \uC27D\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4\
  ."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?
하스켈에서 코드를 함수로 구성하는 것은 코드를 재사용 가능하고 이름이 있는 블록으로 분해하는 것을 의미합니다. 왜 그럴까요? 코드를 DRY(Do not Repeat Yourself, 반복하지 않기)하게 유지하고, 가독성을 높이며, 디버깅을 더 쉽게 만듭니다.

## 방법:
다음은 하스켈에서 함수를 작성하고 사용하는 방법입니다:

```Haskell
-- 두 수를 더하는 간단한 함수 정의
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- 함수 사용하기
main = print (addNumbers 3 5)
```

출력:
```
8
```

고차 함수도 만들 수 있습니다:

```Haskell
-- 함수를 받아서 어떤 것에 두 번 적용
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 익명 함수를 사용한 applyTwice
main = print (applyTwice (*2) 5)
```

출력:
```
20
```

## 심화 탐구
하스켈은 순수 함수형 언어로, 함수를 일급 객체로 취급합니다. 역사적으로 이는 계산의 기초적 틀인 람다 계산법에 뿌리를 두고 있습니다. 명령형 언어에서 함수가 일련의 지시사항이라면, 하스켈에서 함수는 데이터 간의 관계를 서술하는 표현식입니다.

재사용을 위해 원시 함수를 작성하는 것 외에도 대안이 있습니다. 다형성을 위해 타입 클래스를 사용하는 것을 고려하거나, 관련 함수를 그룹화하기 위해 모듈을 활용하세요. 하스켈의 지연 평가도 함수 구현에 영향을 미칩니다—함수 결과가 필요할 때까지 평가되지 않으므로, 성능 고려 사항에 영향을 줄 수 있습니다.

## 참고 자료
- 공식 하스켈 문서: https://www.haskell.org/documentation/
- 초보자에게 친절한 책 "Learn You a Haskell for Great Good!" by Miran Lipovača: http://learnyouahaskell.com/
- "Real World Haskell" by Bryan O'Sullivan, Don Stewart, John Goerzen: http://book.realworldhaskell.org/
