---
title:    "Haskell: 디렉토리가 존재하는지 확인하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 하다보면 때때로 특정 디렉토리가 존재하는지 확인해야할 때가 있습니다. 제대로 된 디렉토리 경로를 사용하는지를 확인하고, 작성한 코드를 더 견고하게 만들기 위해 이 작업이 필요합니다.

## 어떻게

우리는 Haskell의 built-in 함수인 `doesDirectoryExist`를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 다음과 같이 간단한 코드를 작성해보겠습니다.

```Haskell
import System.Directory

main = do
    let directory = "/Users/john/documents"
    exists <- doesDirectoryExist directory
    if exists
        then putStrLn "This directory exists."
        else putStrLn "This directory does not exist."
```

위 코드는 주어진 디렉토리 경로가 존재하는지를 확인하고, 그에 따라 적절한 메시지를 출력합니다. 예제 출력은 다음과 같습니다.

```
This directory exists.
```

만약 존재하지 않는 디렉토리 경로를 사용하면 다음과 같은 결과가 나올 것입니다.

```
This directory does not exist.
```

위의 예제에서 `exists` 변수는 불리언 값으로 디렉토리가 존재하는지 여부를 나타냅니다. 이 값은 `if` 문에서 조건으로 사용되어 적절한 메시지를 출력하는 데 사용됩니다.

## 깊게 들어가기

Haskell의 `System.Directory` 모듈에는 디렉토리와 파일에 대한 여러 기능이 포함되어 있습니다. `doesDirectoryExist` 함수 외에도 `createDirectory` 함수로 디렉토리를 생성하거나, `getCurrentDirectory` 함수로 현재 작업 디렉토리를 가져올 수 있습니다.

하지만 디렉토리의 존재 여부를 확인하는 것이 언제나 바람직한것은 아닙니다. 예를 들어, 파일에 접근하려는 경우에는 파일의 존재 여부를 확인하는 것이 필요합니다. 이를 위해 `System.Directory` 모듈에는 `doesFileExist` 함수가 있습니다. 

## 관련 자료

- [Haskell Documentation: System.Directory](https://www.haskell.org/onlinereport/io.html#directory)
- [Real World Haskell: Filesystem Operations](http://book.realworldhaskell.org/read/io.html#filesystem)
- [Hackage: directory package](https://hackage.haskell.org/package/directory)