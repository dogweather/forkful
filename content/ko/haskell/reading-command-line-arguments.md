---
title:                "Haskell: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

이 글을 읽는 이유는 무엇일까요? 우리가 Haskell 프로그래밍을 하다보면 종종 커맨드 라인 인자를 다루어야 하는 경우가 있습니다. 이 글에서는 인자를 읽는 방법을 다루고, 이를 통해 더 나은 프로그램을 만드는 방법을 알아보겠습니다.

## 어떻게

커맨드 라인 인자를 다루는 것은 매우 간단합니다. Haskell의 `System.Environment` 모듈에는 `getArgs`라는 함수가 있습니다. 이 함수는 `IO [String]` 타입을 반환하며, 커맨드 라인에서 입력된 인자들의 리스트를 제공합니다.

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("총 " ++ show (length args) ++ "개의 인자가 입력되었습니다.")
  putStrLn "인자 리스트:"
  mapM_ putStrLn args
```

위 예제에서 `getArgs` 함수를 사용하여 인자를 읽고, `length` 함수를 통해 총 인자의 개수를 구한 뒤, `putStrLn`을 사용하여 결과를 출력합니다.

이제 컴파일 후 커맨드 라인에서 다음과 같이 입력해보세요.

```
$ ./example arg1 arg2 arg3
```

아래와 같은 결과가 출력될 것입니다.

```
총 3개의 인자가 입력되었습니다.
인자 리스트:
arg1
arg2
arg3
```

## 심층 분석

`System.Environment` 모듈은 `getArgs` 함수 외에도 여러 가지 유용한 함수를 제공합니다. 예를 들어, `getProgName` 함수는 현재 실행중인 프로그램의 이름을 반환합니다. 이를 활용하면 프로그램의 이름에 따라 다르게 동작하는 프로그램을 만들 수 있습니다. 또한, `getEnv` 함수를 사용하면 환경 변수를 읽어올 수도 있습니다.

더 깊이 들어가서 다양한 함수들을 살펴보면서, 커맨드 라인 인자 외에도 다양한 환경 정보를 읽고 활용하는 방법을 알아볼 수 있습니다. 하지만 이러한 함수들을 자세히 다루는 것은 다른 글의 주제이므로 여기서는 패스하겠습니다.

## 서로 참고하기

- [Haskell 기본 가이드](https://www.haskell.org/documentation)
- [Haskell 튜토리얼](https://www.tutorialspoint.com/haskell/index.htm)
- [Haskell 프로그래밍 입문](https://en.wikibooks.org/wiki/Haskell/Getting_started)