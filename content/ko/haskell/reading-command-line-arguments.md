---
title:    "Haskell: 커맨드 라인 인수 읽기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜?

프로그래밍을 하다보면 종종 우리는 다양한 방법으로 사용자와 상호 작용해야 할 때가 있습니다. 예를 들어 사용자로부터 입력을 받거나 스크립트의 동작을 호출하는 등의 상황이 있을 수 있습니다. 이때 command line arguments는 유용한 도구가 될 수 있습니다. 이번 블로그 포스트에서는 Haskell에서 command line arguments를 읽는 방법에 대해 알아보겠습니다.

## 어떻게?

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs     -- 입력된 argument들을 받아옴
  putStrLn "인사말: "  -- 사용자로부터 입력 받기 전에 인사말을 출력
  name <- getLine    -- 사용자로부터 입력 받음
  putStrLn $ "Hello, " ++ name ++ "!"  -- 입력 받은 이름에 맞는 인사말 출력
```

위의 코드를 통해 우리는 `System.Environment` 라이브러리를 사용해 사용자로부터 입력 받고, 그 입력 값을 활용하는 방법을 배웠습니다. 이 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```
$ runhaskell hello.hs John
인사말: 
Hello, John!
```

위의 예제에서는 사용자로부터 입력 받는 대신에 command line arguments를 이용해 값을 전달받았습니다. 이를 통해 프로그램의 실행에 있어서 더 유연하게 대처할 수 있게 됩니다.

## 깊게 들어가기

이번 섹션에서는 좀 더 심화적인 내용을 다루겠습니다. 위의 예제에서는 `System.Environment` 라이브러리를 사용해 command line arguments를 읽었습니다. 이 라이브러리는 더 많은 함수를 제공하지만 우리가 가장 많이 사용할만한 함수는 `getArgs`와 `getProgName`일 것입니다. `getArgs`는 `IO [String]` 타입을 반환하며, 이는 입력된 argument들을 리스트 형태로 반환합니다. `getProgName`은 `IO String` 타입을 반환하며, 이는 현재 실행 중인 프로그램의 이름을 반환합니다.

또한 우리가 인자를 입력하지 않았을 때 발생할 수 있는 오류에 대해서도 생각해야 합니다. 이때 사용할 수 있는 함수는 `init`과 `last`입니다. `init` 함수는 리스트에서 마지막 요소를 제외한 나머지 리스트를 반환하고, `last` 함수는 리스트에서 마지막 요소를 반환합니다. 이를 활용해서 우리는 다음과 같이 오류처리를 할 수 있습니다.

```Haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  -- 만약 인자를 입력하지 않았을 때
  if last args == progName then
    putStrLn "이름을 입력해주세요."
  else do
    putStrLn $ "Hello, " ++ init args ++ "!"
```

## 참고자료

- [Haskell Documentation](https://www.haskell.org/documentation/) - Haskell 공식 문서 웹사이트
- [Real World Haskell](http://book.realworldhaskell.org/read/) - Haskell에 대한 실전적인 입문서
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - 예제 중심으로 Haskell을 배울 수 있는 책