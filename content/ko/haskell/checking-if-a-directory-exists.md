---
title:    "Haskell: 디렉토리의 존재 여부 확인하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

사용자가 디렉토리가 존재하는지 확인하는 것에 어떠한 이유로 참여해야 하는지에 대해 설명합니다.

디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 중요한 역할을 합니다. 여러분의 애플리케이션에서 파일을 다루거나 데이터를 저장할 때, 디렉토리가 존재하는지 확인하는 것은 잘못된 파일을 덮어쓰는 오류를 방지할 수 있습니다. 또한, 디렉토리가 존재하지 않을 경우 새로운 디렉토리를 만들 수 있는 기회를 제공합니다.

## 하우 투

```Haskell
import System.Directory

main = do
    exists <- doesDirectoryExist "my_directory"
    if exists 
        then putStrLn "디렉토리가 존재합니다."
        else putStrLn "디렉토리가 존재하지 않습니다."
```

위의 예제는 System.Directory 모듈의 doesDirectoryExist 함수를 사용하여 디렉토리가 존재하는지 확인하는 방법을 보여줍니다. 함수의 반환 값은 Boolean 형태로, 디렉토리가 존재하면 True를 반환하고 존재하지 않으면 False를 반환합니다.

다만, 이 방법은 상대적인 디렉토리 경로를 지정할 경우에는 정확한 결과를 얻지 못할 수 있습니다. 따라서 절대 경로를 사용해야 합니다.

## 딥 다이브

디렉토리가 존재하는지 체크하는 것이 어떻게 작동하는지에 대해 더 깊이 알아보겠습니다. System.Directory 모듈의 hasPermission 함수를 사용하여 디렉토리의 읽기 및 쓰기 권한을 확인할 수 있습니다. 이 함수는 두 개의 인자를 받는데, 하나는 권한 종류를 나타내는 문자열이고 다른 하나는 디렉토리 경로입니다.

```Haskell
import System.Directory

main = do
    readPerm <- hasPermission "r" "my_directory"
    writePerm <- hasPermission "w" "my_directory"
    if readPerm && writePerm
        then putStrLn "디렉토리에 쓰고 읽기 권한이 모두 있습니다."
        else if readPerm
            then putStrLn "디렉토리에 읽기 권한만 있습니다."
            else putStrLn "디렉토리에 쓰기 권한만 있습니다."
```

## 봉사 바랍니다

 "System.Directory" 모듈에 대한 자세한 내용은 [공식 문서](https://hackage.haskell.org/package/directory/docs/System-Directory.html#g:6)를 참고하세요. 또한 아래 링크를 통해 디렉토리를 다루는 다른 유용한 함수들을 알아보세요.

- [createDirectory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#g:2)
- [removeDirectory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#g:4)
- [createDirectoryIfMissing](https://hackage.haskell.org/package/directory/docs/System-Directory.html#g:3)

Happy coding!