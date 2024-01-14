---
title:                "Haskell: 디렉토리 존재 여부 확인하기"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 디렉토리가 존재하는지 확인하는 것이 중요한가요?
디렉토리가 존재하는지 여부를 확인하는 것은 중요한 이유가 있습니다. 예를 들어, 파일을 읽고 쓰기 위해 해당 디렉토리에 액세스해야 할 필요가 있을 수 있고, 또는 디렉토리의 존재 여부에 따라 프로그램의 동작이 달라질 수 있습니다.

## 어떻게 디렉토리의 존재 여부를 확인할 수 있나요?
Haskell에서는 `System.Directory` 모듈에 있는 `doesDirectoryExist` 함수를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 아래는 이 함수를 이용한 간단한 예제 코드입니다.

```Haskell
import System.Directory

main = do
    dirExists <- doesDirectoryExist "path/to/directory"
    if dirExists
        then putStrLn "Directory exists."
        else putStrLn "Directory does not exist."
```

위의 코드를 실행하면 해당 디렉토리의 존재 여부에 따라 "Directory exists." 또는 "Directory does not exist."가 표시됩니다.

## 디렉토리 존재 여부에 대해 깊이 알아보기
`doesDirectoryExist` 함수는 실제로 해당 디렉토리가 존재하는지를 확인하는 것이 아니라, 해당 디렉토리가 여러 가지 이유로 인해 액세스할 수 없는 경우에도 `True`를 반환합니다. 예를 들어, 디렉토리에 권한이 없거나 디렉토리가 아닌 파일로 참조되는 경우 등입니다. 따라서, 이 함수를 사용하기 전에 반드시 액세스 권한을 확인하고, 해당 디렉토리가 실제로 디렉토리인지 확인해야 합니다.

## 관련 자료
- [Haskell documentation on `System.Directory` module](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell wiki page on dealing with file and directory paths](https://wiki.haskell.org/File_and_Directory_Layout)