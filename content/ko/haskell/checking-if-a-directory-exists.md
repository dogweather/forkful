---
title:                "Haskell: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일 시스템을 다루다보면 때때로 특정 디렉토리가 존재하는지 여부를 확인해야 할 때가 있습니다. 그래서 Haskell을 사용하여 디렉토리의 존재 여부를 확인하는 방법을 배워보겠습니다.

## 어떻게

먼저 System.Directory 모듈을 임포트합니다.

```Haskell
import System.Directory
```

다음으로, `doesDirectoryExist` 함수를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 이 함수는 디렉토리의 경로를 인자로 받아 Bool 값을 반환합니다.

```Haskell
doesDirectoryExist :: FilePath -> IO Bool
```

예를 들어, "photos" 디렉토리가 존재하는지 여부를 확인하는 예제를 살펴보겠습니다.

```Haskell
existed <- doesDirectoryExist "photos"
print existed -- True 혹은 False 값이 출력됩니다.
```

## 깊게 파고들기

이제 `doesDirectoryExist` 함수가 어떻게 동작하는지 살펴보겠습니다. 이 함수는 내부적으로 `fileExist` 함수를 호출하여 디렉토리의 존재 여부를 확인합니다. 만약 해당 경로가 파일인 경우에는 False 값을 반환하며, 그 외에는 디렉토리의 존재 여부를 확인하기 위해 운영 체제에 따라 다른 시스템 호출을 수행합니다.

## 또 다른 정보들

- [System.Directory 모듈 문서](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell에서 파일 시스템 다루기](https://haskell.fpcomplete.com/library/directory)
- [Haskell에서 파일 및 디렉토리 다루기](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/9-file-and-directory-management)