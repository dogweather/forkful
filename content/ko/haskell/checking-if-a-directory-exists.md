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

# 왜?

프로그램을 작성하는 동안 디렉토리가 존재하는지 확인해야 할 수 있습니다. 이를 통해 우리는 프로그램이 예상대로 작동하거나 예외 상황을 처리할 수 있습니다.

## 방법

디렉토리가 있는지 확인하는 가장 간단한 방법은 `doesDirectoryExist` 함수를 사용하는 것입니다. 예를 들어, 우리가 현재 작업 디렉토리에 있는 "test"라는 디렉토리가 있는지 확인하고 싶다면 다음과 같이 작성할 수 있습니다.

```Haskell
import System.Directory

main = do
    isTestDir <- doesDirectoryExist "test"
    if isTestDir
        then putStrLn "test 디렉토리가 있습니다."
        else putStrLn "test 디렉토리가 없습니다."
```

위 코드를 실행하면 "test 디렉토리가 없습니다."라는 결과가 나올 것입니다. 만약 디렉토리가 존재한다면, "test 디렉토리가 있습니다."라는 결과가 나올 것입니다.

## 깊게 들어가기

`doesDirectoryExist` 함수는 `System.Directory` 모듈에서 제공되며, 디렉토리의 존재 여부를 감지하기 위해 시스템의 파일 시스템에 직접 액세스합니다. 이 함수는 인자로 디렉토리의 경로를 받고, 해당 경로의 파일 유형을 알아낸 후 디렉토리인지 아닌지를 판단합니다.

시스템 디렉토리를 아직 작성하지 않은 경우, `System.Directory` 모듈에서 제공하는 함수를 사용하여 디렉토리를 생성할 수도 있습니다. `createDirectory` 함수는 지정된 경로에 디렉토리를 생성하고, 경로를 따라 부모 디렉토리들도 모두 생성합니다.

```Haskell
import System.Directory

main = do
    createDirectory "test/dir"
```

위 코드를 실행하면, "test"라는 디렉토리 아래에 "dir"이라는 디렉토리가 생성될 것입니다. `createDirectoryIfMissing` 함수를 사용하면 디렉토리가 이미 존재하는지 여부를 미리 확인한 후 생성할 수도 있습니다.

## 참고

- [Hackage: System.Directory module](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Learn You A Haskell: System.Directory](http://learnyouahaskell.com/input-and-output#files-and-streams)
- [Haskell Wiki: File Manipulation](https://wiki.haskell.org/File_manipulation)