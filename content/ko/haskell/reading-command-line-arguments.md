---
title:                "컴퓨터 프로그래밍의 '명령 줄 인수 읽기'"
html_title:           "Haskell: 컴퓨터 프로그래밍의 '명령 줄 인수 읽기'"
simple_title:         "컴퓨터 프로그래밍의 '명령 줄 인수 읽기'"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

입력받은 커맨드 라인 인자를 읽는 방법을 배우는 것은 한 컴퓨터 프로그래머로서 중요한 능력입니다. 이를 통해 프로그램에 유동성을 부여하고 사용자와 상호작용하는 기능을 추가할 수 있습니다. 

## 어떻게

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "입력받은 인자는 " ++ show args ++ "입니다."
```

위 코드는 `System.Environment` 모듈을 임포트하고 `getArgs` 함수를 사용하여 입력받은 인자들을 `args` 변수에 할당합니다. 그리고 `putStrLn` 함수를 사용하여 해당 변수의 값을 출력합니다.

입력받은 인자를 사용하여 조건문을 작성하거나 다양한 형태로 가공하는 것도 가능합니다.

또한, 인자의 개수나 특정 인자의 값 등을 확인하고 처리하는 것도 가능합니다.

```Haskell
import System.Environment

main = do
    args <- getArgs
    let numArgs = length args
    if numArgs == 0
        then putStrLn "인자를 입력하지 않았습니다."
        else do
            putStrLn $ "입력받은 인자의 개수는 " ++ show numArgs ++ "개입니다."
            putStrLn $ "첫 번째 인자는 " ++ head args ++ "입니다."
```

위 코드는 `length` 함수를 사용하여 입력받은 인자의 개수를 확인하고, `if`문을 이용하여 인자가 없는 경우와 있는 경우를 구분합니다. `head` 함수를 사용하여 첫 번째 인자의 값을 가져와 출력합니다.

## 딥 다이브

위 코드에서 `getArgs` 함수는 `IO [String]` 타입의 값을 반환합니다. 이는 `IO` 모나드와 `[String]` 리스트 타입의 조합입니다. 이는 프로그래머가 실제로 인자 값을 읽는 시점을 컨트롤 하기 위해 사용되는 메커니즘입니다. 이러한 메커니즘은 side effect를 줄이고 프로그램을 더 안전하게 만들어줍니다. 또한, `getArgs` 함수는 리스트를 사용하기 때문에 `map`, `filter`와 같은 고차함수를 사용하여 더 복잡한 로직을 작성할 수도 있습니다.

## 관련 자료

- [Haskell System.Environment 모듈 문서](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Haskell 입문서](https://wiki.haskell.org/Haskell_in_5_steps)
- [RWH: Chapter 9. Parsing command line arguments](http://book.realworldhaskell.org/read/command-line.html)