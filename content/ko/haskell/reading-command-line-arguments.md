---
title:    "Haskell: 컴퓨터 프로그래밍에 대한 기사 제목: 커맨드 라인 인수 읽기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인수를 읽는 것에 대해서는 많은 이유가 있습니다. 첫째, 프로그램을 실행할 때 다른 설정 값을 전달하고 싶을 때 등 여러 용도로 사용할 수 있습니다.

## 사용 방법
커맨드 라인 인수를 읽는 것은 간단합니다. `System.Environment` 모듈에서 제공하는 `getArgs` 함수를 사용하면 됩니다. 이 함수는 `IO [String]` 타입을 반환하므로 결과를 얻기 위해서는 `IO` 모나드 내에서 작성해야 합니다.

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("입력받은 인수: " ++ show args)
```

위 예제 코드를 컴파일하고 실행하면 아래와 같은 결과를 얻을 수 있습니다.

```
$ ghc argument_example.hs
$ ./argument_example Hello World
입력받은 인수: ["Hello", "World"]
```

## 깊이 들어가기
커맨드 라인 인수를 읽을 때 유의해야 할 몇 가지 사항이 있습니다.

- `getArgs` 함수는 인수를 리스트 형태로 반환합니다. 이 때, 인수 마다 따옴표가 붙어 반환되는데, `show` 함수를 사용하면 해당 따옴표가 포함된 채로 문자열로 변환됩니다. 따라서, 인수를 파싱할 때에는 따옴표를 제거해주는 작업이 필요합니다.
- 만약 프로그램을 실행할 때 인수를 전달하지 않으면 `getArgs` 함수는 빈 리스트 `[]`를 반환합니다. 따라서, 이에 대한 예외 처리를 해주는 것이 좋습니다.

## 관련 자료
- [Haskell 공식 문서: System.Environment 모듈](https://www.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Haskell Wikibook: Command line arguments](https://en.wikibooks.org/wiki/Haskell/Command_line_arguments)
- [Haskell By Example: Reading Command Line Arguments](https://lotz84.github.io/haskellbyexample/ex/reading-command-line-arguments)