---
date: 2024-01-26 01:10:51.942117-07:00
description: "\uBC29\uBC95: \uD558\uC2A4\uCF08\uC740 \uC21C\uC218 \uD568\uC218\uD615\
  \ \uC5B8\uC5B4\uB85C, \uD568\uC218\uB97C \uC77C\uAE09 \uAC1D\uCCB4\uB85C \uCDE8\uAE09\
  \uD569\uB2C8\uB2E4. \uC5ED\uC0AC\uC801\uC73C\uB85C \uC774\uB294 \uACC4\uC0B0\uC758\
  \ \uAE30\uCD08\uC801 \uD2C0\uC778 \uB78C\uB2E4 \uACC4\uC0B0\uBC95\uC5D0 \uBFCC\uB9AC\
  \uB97C \uB450\uACE0 \uC788\uC2B5\uB2C8\uB2E4. \uBA85\uB839\uD615 \uC5B8\uC5B4\uC5D0\
  \uC11C \uD568\uC218\uAC00 \uC77C\uB828\uC758 \uC9C0\uC2DC\uC0AC\uD56D\uC774\uB77C\
  \uBA74, \uD558\uC2A4\uCF08\uC5D0\uC11C \uD568\uC218\uB294 \uB370\uC774\uD130 \uAC04\
  \uC758 \uAD00\uACC4\uB97C \uC11C\uC220\uD558\uB294 \uD45C\uD604\uC2DD\uC785\uB2C8\
  \uB2E4. \uC7AC\uC0AC\uC6A9\uC744 \uC704\uD574 \uC6D0\uC2DC \uD568\uC218\uB97C\u2026"
lastmod: '2024-04-05T22:51:09.630288-06:00'
model: gpt-4-1106-preview
summary: "\uD558\uC2A4\uCF08\uC740 \uC21C\uC218 \uD568\uC218\uD615 \uC5B8\uC5B4\uB85C\
  , \uD568\uC218\uB97C \uC77C\uAE09 \uAC1D\uCCB4\uB85C \uCDE8\uAE09\uD569\uB2C8\uB2E4\
  ."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

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
