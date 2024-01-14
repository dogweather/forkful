---
title:                "Haskell: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜?

Haskell 프로그래밍을 할 때 종종 일시적인 파일을 생성해야 할 일이 있습니다. 임시 파일은 프로그램의 일시적인 데이터를 저장하기에 유용합니다.

## 어떻게?

임시 파일을 생성하는 방법은 간단합니다. 먼저 `System.IO.Temp` 모듈을 임포트합니다. 그리고 `withSystemTempFile` 함수를 사용하여 임시 파일을 생성합니다.

```Haskell
import System.IO.Temp

main = withSystemTempFile "myfile.txt" $ \fp handle -> do
  -- 파일 경로와 핸들을 인자로 받아 임시 파일을 생성합니다.
  -- 파일 처리 코드를 작성합니다.
  hPutStr handle "Hello World!"
  -- 파일을 닫습니다.
  hClose handle
```

위의 코드에서 `withSystemTempFile` 함수는 임시 파일을 생성하고 파일 경로와 핸들을 인자로 받는 함수를 실행합니다. 이후 파일 처리 코드를 작성할 수 있습니다. 마지막으로 파일을 닫으면 임시 파일이 자동으로 삭제됩니다.

이제 임시 파일이 생성되었습니다. 이를 확인하기 위해 아래와 같이 임시 파일의 내용을 출력해보겠습니다.

```Haskell
import System.IO.Temp

main = withSystemTempFile "myfile.txt" $ \fp handle -> do
  -- 파일을 읽고 출력합니다.
  content <- hGetContents handle
  putStrLn content
  -- 파일을 닫습니다.
  hClose handle
```

실행 결과는 다음과 같습니다.

```
Hello World!
```

## 깊게 파보기

임시 파일을 생성하는 `withSystemTempFile` 함수는 실제로 시스템 함수들을 사용하여 작업을 수행합니다. 이를 통해 임시 파일을 생성하는 법을 좀 더 깊이 파악할 수 있습니다.

`withSystemTempFile`의 정의는 다음과 같습니다.

```Haskell
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
```

첫 번째 인자는 생성할 임시 파일의 이름, 두 번째 인자는 파일 경로와 핸들을 인자로 받는 함수입니다. 이 함수를 실행하면 임시 파일이 생성된 후, 인자로 받은 함수가 실행됩니다. 마지막으로 반환된 결과 값이 `withSystemTempFile`의 반환 값이 됩니다.

`withSystemTempFile`은 `withTempDirectory` 함수를 사용하여 임시 디렉토리를 생성하고, 생성된 디렉토리 안에 임시 파일을 생성합니다. 이후 인자로 받은 함수를 실행하고, 모든 작업이 끝난 후 임시 디렉토리와 파일을 삭제합니다.

## 더 알아보기

- [Haskell documentation for System.IO.Temp module](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Temp.html)
- [Creating temporary files in Haskell: A beginner's guide](https://www.codementor.io/@shajalahamed543/creating-temporary-files-haskell-beginner-s-guide-f0n5xx4ve)
- [Haskell: Create a temporary file or directory](https://www.beyondgrep.com/2014/04/07/haskell-create-a-temporary-file-or-directory/)