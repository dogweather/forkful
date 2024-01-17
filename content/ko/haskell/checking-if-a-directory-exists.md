---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Haskell: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
디렉토리가 존재하는지 확인하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 이를 통해 우리는 프로그램이 참조하는 파일들이 실제로 존재하는지 확인할 수 있습니다. 또한 해당 디렉토리에 대한 권한이 있는지도 확인할 수 있습니다.

## 방법:
우리를 지원하는 사용 가능한 함수가 있습니다:
* `doesDirectoryExist :: FilePath -> IO Bool` - 주어진 경로의 디렉토리가 존재하는지 확인합니다.
* `doesDirectoryExist "경로"` - 주어진 경로에 디렉토리가 존재하는지 확인하여 `참` 또는 `거짓`을 반환합니다.

```Haskell
  import System.Directory (doesDirectoryExist)
  main :: IO ()
  main = do
    exist <- doesDirectoryExist "경로"
    if exist
      then putStrLn "해당 디렉토리가 존재합니다."
      else putStrLn "해당 디렉토리는 존재하지 않습니다."
```

## 디프 다이브:
이 함수는 `System.Directory` 모듈에 포함되어 있습니다. 또한 `System.Posix.Files` 모듈에서도 디렉토리가 존재하는지 확인할 수 있는 함수들이 있습니다. 예를 들어, `getFileStatus` 함수는 파일 또는 디렉토리에 대한 상세한 정보를 제공합니다.

## 관련 자료:
* [Haskell documentation for `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
* [Haskell documentation for `System.Posix.Files`](https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html)