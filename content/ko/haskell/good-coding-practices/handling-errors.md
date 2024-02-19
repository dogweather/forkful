---
aliases:
- /ko/haskell/handling-errors/
date: 2024-01-26 00:53:59.346402-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC5D0\uB7EC\uB97C \uCC98\
  \uB9AC\uD558\uB294 \uAC83\uC740 \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744\
  \ \uAD00\uB9AC\uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4\
  \u2014\uC798\uBABB\uB418\uC5C8\uC744 \uC218 \uC788\uB294 \uAC83\uB4E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uADF8\uB4E4\uC758 \uD504\uB85C\uADF8\uB7A8\uC774 \uC774\
  \uB7F0 \uC0C1\uD669\uB4E4\uC744 \uC6B0\uC544\uD558\uAC8C \uB2E4\uB8F0 \uC218 \uC788\
  \uB3C4\uB85D \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574, \uCDA9\uB3CC \uC5C6\uC774 \uB610\
  \uB294 \uC798\uBABB\uB41C \uACB0\uACFC\uB97C \uB0B4\uC9C0 \uC54A\uB3C4\uB85D \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.298507
model: gpt-4-1106-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC5D0\uB7EC\uB97C \uCC98\uB9AC\
  \uD558\uB294 \uAC83\uC740 \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uAD00\
  \uB9AC\uD558\uB294 \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4\u2014\
  \uC798\uBABB\uB418\uC5C8\uC744 \uC218 \uC788\uB294 \uAC83\uB4E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uADF8\uB4E4\uC758 \uD504\uB85C\uADF8\uB7A8\uC774 \uC774\uB7F0\
  \ \uC0C1\uD669\uB4E4\uC744 \uC6B0\uC544\uD558\uAC8C \uB2E4\uB8F0 \uC218 \uC788\uB3C4\
  \uB85D \uBCF4\uC7A5\uD558\uAE30 \uC704\uD574, \uCDA9\uB3CC \uC5C6\uC774 \uB610\uB294\
  \ \uC798\uBABB\uB41C \uACB0\uACFC\uB97C \uB0B4\uC9C0 \uC54A\uB3C4\uB85D \uC774\uB97C\
  \ \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
프로그래밍에서 에러를 처리하는 것은 예상치 못한 상황을 관리하는 것과 관련이 있습니다—잘못되었을 수 있는 것들. 프로그래머는 그들의 프로그램이 이런 상황들을 우아하게 다룰 수 있도록 보장하기 위해, 충돌 없이 또는 잘못된 결과를 내지 않도록 이를 수행합니다.

## 방법:
하스켈은 `Maybe` 및 `Either`와 같은 타입을 통해 강력한 에러 처리를 합니다. 간단한 살펴보기입니다:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- 0으로 나누는 것은 안됩니다, 그래서 Nothing을 반환합니다.
safeDivide x y = Just (x `div` y)  -- 그 외에는 모두 좋습니다, 결과를 Just에 넣어서 반환합니다.

-- 실행 예시:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

더 복잡한 에러 처리를 위해 `Either`를 사용합니다:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- 이번에는 에러가 메시지를 가지고 있습니다.
safeDivideEither x y = Right (x `div` y)

-- 사용 예시:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## 심도 있는 탐구
하스켈 세계에서, 에러 처리는 강력한 역사를 가지고 있습니다. 옛날에는, 에러가 전체 프로그램을 망가뜨릴 수 있었습니다—재미없죠. 하스켈의 타입 시스템은 이를 훨씬 덜 가능하게 만드는 방법을 제공합니다. 우리는 `Maybe`와 `Either`를 가지고 있지만, 다른 시나리오에 대한 `Exceptions`나 `IO`와 같은 다른 것들도 있습니다.

`Maybe`는 간단합니다: 모든 것이 잘 되었을 때는 `Just` 무언가를 받고, 그렇지 않으면 `Nothing`을 받습니다. `Either`는 한 단계 더 나아가, 에러 메시지(`Left`) 또는 성공적인 결과(`Right`)를 반환할 수 있게 합니다.

둘 다 순수하며, 외부 세계에 영향을 끼치지 않습니다 – 하스켈에서 큰 문제입니다. 우리는 다른 언어들을 괴롭히는 검사되지 않은 예외의 함정을 피합니다.

`Maybe`와 `Either`로 만족하지 않는 사람을 위해, `Control.Exception`과 같은 라이브러리들은 예외를 통해 더 전통적인, 명령형 스타일의 에러 처리를 제공합니다. 하지만 이를 너무 자유롭게 사용하는 것은 복잡해질 수 있으므로, 커뮤니티는 종종 타입을 사용하는 것을 선호합니다.

## 더 보기
더 깊이 탐구하려면 다음을 참조하세요:

- 하스켈 자체 문서: [하스켈](https://haskell.org/documentation)
- 초보자에게 좋습니다: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
