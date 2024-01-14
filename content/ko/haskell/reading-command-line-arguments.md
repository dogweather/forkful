---
title:                "Haskell: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

Haskell을 사용하면서 가끔 커맨드 라인 인자를 읽어야 할 때가 있습니다. 따라서 이를 어떻게 수행할 수 있는지 알아봅시다!

## 어떻게 수행할까요?

명령줄 인자를 읽기 위해서는 `System.Environment` 모듈의 `getArgs` 함수를 사용합니다. 이 함수는 `IO [String]` 타입을 반환하며, 커맨드 라인에서 전달된 인자들의 리스트를 가져옵니다. 예를 들어, 다음과 같이 코드를 작성하면 됩니다.

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("인자들: " ++ show args)
```

위 코드를 `getArgs_example.hs` 라는 파일에 저장한 후, 커맨드 라인에서 `runghc` 명령어를 사용하여 실행해보겠습니다.

```
runghc getArgs_example.hs hello 123 "안녕하세요"
```

출력 결과는 다음과 같을 것입니다.

```
인자들: ["hello", "123", "안녕하세요"]
```

위와 같이, `getArgs` 함수를 이용하여 커맨드 라인에서 전달된 인자들을 읽을 수 있습니다. 인자들이 문자열로 저장되는 것에 주의해야 합니다.

## 딥 다이브

커맨드 라인 인자를 읽는 과정에서 더 많은 옵션을 제공하는 몇 가지 함수들이 있습니다. 예를 들어, `getProgName` 함수는 프로그램의 이름, 즉 실행하는 파일의 이름을 반환합니다. 또한, `getEnvironment` 함수를 이용하면 컴퓨터의 모든 환경 변수를 읽어올 수 있습니다. 자세한 내용은 Haskell 문서를 참고하시기 바랍니다.

## See Also

- [Haskell 문서: System.Environment 모듈](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Haskell 문서: getArgs 함수](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#v:getArgs)
- [Haskell 문서: getProgName 함수](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#v:getProgName)
- [Haskell 문서: getEnvironment 함수](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#v:getEnvironment)