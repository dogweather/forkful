---
title:                "임시 파일 생성하기"
html_title:           "Haskell: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만들고 사용하는 이유는 주로 프로그램이나 스크립트에서 일시적으로 데이터를 저장하고 다루기 위해서입니다. 이러한 임시 파일은 프로그램 실행 중에 일시적으로 생성되고 사용되며, 사용이 끝나면 자동으로 삭제됩니다.

## 방법

임시 파일을 만드는 가장 간단한 방법은 `withSystemTempFile` 함수를 사용하는 것입니다. 이 함수는 임시 파일을 만들고 파일의 경로와 핸들을 전달 받은 뒤, 지정된 작업을 수행한 후에 임시 파일을 자동으로 삭제합니다.

```Haskell
import System.IO.Temp (withSystemTempFile)

-- 임시 파일의 경로와 핸들을 전달받아 사용하는 예제
main = withSystemTempFile "temp.txt" $ \path handle -> do
  hPutStrLn handle "Hello world!"
  hClose handle
  putStrLn $ "임시 파일 경로: " ++ path
```

프로그램을 실행하면 "temp.txt" 파일에 "Hello world!" 문자열이 쓰여지고, "임시 파일 경로: ..." 형태의 메세지가 출력됩니다.

## 속이기

임시 파일을 생성하는 더 깊은 내용을 살펴보기 전에 임시 파일의 경로를 어떻게 생성되는지 살펴보겠습니다. `withSystemTempFile` 함수는 `IO` 모나드를 사용하여 임시 파일의 경로를 생성합니다. 다시 말해, 임시 파일은 프로그램이 실행되는 기기에 따라 다르게 설정될 수 있습니다.

경로 생성 방법을 더 세부적으로 설정하려면 `withSystemTempFile` 대신 `withTempFile` 함수를 사용할 수 있습니다. 이 함수는 `FilePath` 타입의 `tmpDir` 인자를 전달받아 임시 폴더를 지정할 수 있습니다.

```Haskell
import System.IO.Temp (withTempFile)

{- 사용자가 지정한 임시 폴더에서 임시 파일을 만드는 예제 -}
main = withTempFile "C:\\Temp\\" "temp.txt" $ \path handle -> do
  hPutStrLn handle "Hello world!"
  hClose handle
  putStrLn $ "임시 파일 경로: " ++ path
```

프로그램을 실행하면 "C:\Temp\temp.txt" 파일에 "Hello world!" 문자열이 쓰여지고, "임시 파일 경로: ..." 메세지가 출력됩니다.

## 더 들어가기

임시 파일은 `withSystemTempFile` 함수를 사용하여 쉽게 생성하고 사용할 수 있지만, 임시 파일을 직접 다루는 방법을 알아두는 것도 중요합니다. 임시 파일 핸들을 사용하여 파일을 읽고 쓰는 방법을 익히는 것은 프로그래머로서 필수적인 기술입니다.

또한, 이 임시 파일들을 자동으로 삭제하는 방법도 알아 두는 것이 좋습니다. `withSystemTempFile` 함수는 자동 삭제를 해주지만, 직접 삭제를 해줘야하는 경우를 대비하여 임시 파일을 삭제하는 방법도 알아두는 것이 좋습니다.

## 더 읽어보기

- [Haskell Document Template System](https://hackage.haskell.org/package/doclayout) - 임시 파일을 만드는 더 많은 옵션을 제공하는 라이브러리
- [Using Temporary Files in Haskell](https://mpickering.github.io/posts/2016-07-21-temporary-files.html) - 임시 파일을 만들고 다루는 더 많은 예제와 정보를 제공하는 블로그 포스트