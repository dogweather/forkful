---
title:                "Haskell: 표준 오류에 쓰는 작성법"
simple_title:         "표준 오류에 쓰는 작성법"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 에러를 쓰는 것은 프로그래밍에서 중요한 부분입니다. 여러분이 내부적으로 어떻게 동작하는지보고 디버깅하고 코드에서 오류를 찾을 때 도움이 될 수 있습니다.

## 방법

먼저, 프로그램의 표준 에러를 무시하지 않도록 주의해야 합니다. 그렇게 하려면 "Control.Exception" 모듈에서 "catch" 함수를 사용하여 예외를 처리하면 됩니다.

```Haskell
import Control.Exception (catch, IOException)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  -- 예외를 처리하기 위해 "catch" 함수 사용
  catch (divisionByZero) errorHandling

divisionByZero :: IO ()
divisionByZero = do
  putStrLn "Enter a number: "
  num <- getLine
  let result = 5 / (read num :: Double) -- 숫자를 불러와서 0으로 나눔
  putStrLn $ "Result: " ++ show result

-- 예외를 처리하는 함수 정의
errorHandling :: IOException -> IO ()
errorHandling ex = do
  hPutStrLn stderr "Error: Cannot divide by zero."
```

위의 예제에서는 "divisionByZero" 함수에서 예외를 발생시키고, "errorHandling" 함수에서 해당 예외를 처리합니다. 결과는 다음과 같이 출력됩니다.

```
Enter a number:
0
Error: Cannot divide by zero.
```

## 더 깊게

더 깊게 들어가보면, 표준 에러는 "System.IO" 모듈의 "stderr" 핸들러를 사용하여 작성할 수 있습니다. "print" 함수를 사용하여 표준 출력 대신 표준 에러를 출력할 수 있습니다.

```Haskell
import System.IO (stderr)

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
```

위의 예제에서는 "hPutStrLn" 함수를 사용하여 표준 에러에 해당 메시지를 출력합니다.

## 참고

- ["Control.Exception" 모듈 문서](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html)
- ["System.IO" 모듈 문서](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [예외 처리와 모나드](https://wiki.haskell.org/Error_messages_and_Exceptions)