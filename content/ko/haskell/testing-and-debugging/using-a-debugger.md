---
date: 2024-01-26 03:50:20.531961-07:00
description: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uAC80\uC0AC, \uC77C\uC2DC \uC815\uC9C0, \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589\
  \ \uC911\uAC04\uC5D0 \uC870\uC791\uD560 \uC218 \uC788\uB294 \uB3C4\uAD6C\uB85C \uCF54\
  \uB4DC\uB97C \uD30C\uACE0\uB4DC\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uBC84\uADF8\uB97C \uCD94\uC801\uD558\uACE0\
  , \uD504\uB85C\uADF8\uB7A8 \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uC790\uC2E0\
  \uC758 \uCF54\uB4DC\uAC00 \uC815\uD655\uD788 \uAE30\uB300\uD55C \uB300\uB85C \uB3D9\
  \uC791\uD558\uB294\uC9C0 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\
  \uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.296479-07:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uAC80\
  \uC0AC, \uC77C\uC2DC \uC815\uC9C0, \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uC911\uAC04\
  \uC5D0 \uC870\uC791\uD560 \uC218 \uC788\uB294 \uB3C4\uAD6C\uB85C \uCF54\uB4DC\uB97C\
  \ \uD30C\uACE0\uB4DC\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uBC84\uADF8\uB97C \uCD94\uC801\uD558\uACE0, \uD504\uB85C\
  \uADF8\uB7A8 \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uC790\uC2E0\uC758 \uCF54\
  \uB4DC\uAC00 \uC815\uD655\uD788 \uAE30\uB300\uD55C \uB300\uB85C \uB3D9\uC791\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\
  \uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용한다는 것은 검사, 일시 정지, 프로그램 실행 중간에 조작할 수 있는 도구로 코드를 파고드는 것을 의미합니다. 프로그래머는 버그를 추적하고, 프로그램 흐름을 이해하며, 자신의 코드가 정확히 기대한 대로 동작하는지 확인하기 위해 이를 수행합니다.

## 사용 방법:
GHCi, Haskell의 대화형 환경을 이용한 산책을 함께해 보겠습니다. GHCi는 기본 디버거로 활동할 수 있습니다. Haskell 코드와 함께 GHCi를 실행하고 주변을 살펴봅니다. 예를 들어보겠습니다:

```Haskell
main :: IO ()
main = do
    putStrLn "Hey, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "! Let's debug."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- 여기에 버그가 있다고 가정해 보세요
```

GHCi를 사용하여 디버깅을 시작하려면:

```bash
$ ghci YourHaskellFile.hs
```

`buggyFunction`에서 중단점을 설정합니다:

```Haskell
Prelude> :break buggyFunction
```

프로그램을 실행합니다:

```Haskell
Prelude> :main
Hey, what's your name?
```

프로그램이 `buggyFunction`에서 일시 정지됩니다. 이제 변수를 검사하고, 코드를 단계별로 진행하며, 표현식을 평가할 수 있습니다.

## 심층 탐구:
역사적으로, Haskell의 순수 함수와 강력한 타입 시스템은 디버깅 도구가 그다지 중요하지 않다는 믿음으로 이어졌습니다. 현실은 다릅니다—복잡한 프로그램은 항상 좋은 디버깅 도구의 혜택을 받습니다. GHCi는 기본 디버깅 명령을 제공합니다. 그러나 더 시각적인 경험이나 대규모 애플리케이션을 위해서는, Haskell 확장 기능이 있는 Visual Studio Code나 IntelliJ의 Haskell 플러그인과 같은 통합 디버거가 있는 IDE를 탐색해볼 수 있습니다.

디버거 대신 사용할 수 있는 방법으로는 "printf 디버깅"으로 알려진 print 문 사용, Haskell의 강력한 타입 시스템을 활용하여 잘못된 상태를 표현할 수 없게 하는 것 등이 있습니다. 그럼에도 불구하고, 때때로 코드를 단계별로 진행하는 것을 대신할 수 없습니다.

구현 세부 사항에 대해서는, Haskell의 디버거는 런타임 시스템과 함께 작동합니다. 중단점 처리, 단계 실행, 변수 검사를 할 수 있습니다. 그러나, Haskell은 지연 평가되기 때문에, 일부 상황이 직관적이지 않을 수 있습니다. Haskell 프로그램을 디버깅하는 것은 표현식이 언제, 어떻게 평가되는지 주시하는 것을 종종 의미합니다.

## 참고 자료:
- [GHC 사용자 가이드 - 디버거](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell 플러그인](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
