---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:57:03.277269-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 특정 경로에 폴더가 있는지 검사하는 과정입니다. 존재 여부를 확인함으로써, 프로그래머는 파일 작업이나 설정 로드 시 오류를 방지할 수 있습니다.

## How to: (어떻게:)
```Haskell
import System.Directory (doesDirectoryExist)

checkDirectory :: FilePath -> IO ()
checkDirectory path = do
  exists <- doesDirectoryExist path
  putStrLn $ "Directory " ++ path ++ (if exists then " exists." else " does not exist.")

-- 사용 예
main :: IO ()
main = do
  checkDirectory "/path/to/directory"
```

실행 결과:
```
Directory /path/to/directory exists.
```
또는
```
Directory /path/to/directory does not exist.
```

## Deep Dive (심층 분석)
과거에는 파일 시스템 작업을 위해 POSIX API를 직접 이용하거나 복잡한 foreign function 인터페이스를 사용했습니다. `doesDirectoryExist`은 Haskell의 `System.Directory` 모듈에 포함되어 있으며, 이는 이러한 복잡성을 추상화합니다. `System.Directory` 모듈은 platform-independent하게 설계되어 있기 때문에, 다양한 운영체제에서 일관된 작업을 할 수 있습니다.

대안으로는 `getDirectoryContents`와 같은 함수를 사용해 특정 경로의 내용물을 확인하고, 이를 분석하여 디렉토리 존재 여부를 간접적으로 확인할 수 있습니다. 하지만 이 방법은 `doesDirectoryExist`에 비해 더 복잡하고 비효율적입니다.

구현 세부사항으로는 `doesDirectoryExist` 함수가 내부적으로 시스템 호출을 수행하여 해당 경로가 디렉토리인지 확인합니다. 이는 `isDirectory` 함수와 함께 IO 작업을 다룸으로써 발생하는 사이드이펙트를 Monad를 통해 관리합니다.

## See Also (관련 자료)
- [`System.Directory` Documentation](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html)
- [Haskell IO Tutorial](https://www.haskell.org/tutorial/io.html)
- [Learn You a Haskell for Great Good! by Miran Lipovača](http://learnyouahaskell.com/)
- [Real World Haskell by Bryan O'Sullivan, Don Stewart, and John Goerzen](http://book.realworldhaskell.org/)