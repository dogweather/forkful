---
title:                "'디렉토리의 존재 여부 확인하기'"
html_title:           "Elm: '디렉토리의 존재 여부 확인하기'"
simple_title:         "'디렉토리의 존재 여부 확인하기'"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 이를 검사하는지

여러분이 파일을 읽고 쓰는 작업을 할 때, 포함하는 디렉토리가 실제로 존재하는지 확인하는 것은 중요한 일입니다. 디렉토리가 존재하지 않으면 파일이나 데이터를 읽거나 쓸 수 없기 때문입니다. Elm에서 디렉토리가 존재하는지 확인하는 방법을 알아보겠습니다.

## 어떻게 하는지

디렉토리가 존재하는지 확인하는 것은 매우 간단합니다. Elm의 내장 함수 중 하나인 ```Filesystem.exists```를 사용하면 됩니다. 이 함수에 디렉토리의 경로를 매개변수로 전달하면, 해당 디렉토리가 존재하면 ```True```를, 존재하지 않으면 ```False```를 반환합니다.

```Elm
import Filesystem exposing (exists)

directoryPath : String
directoryPath = "./my_directory"

exists(directoryPath) -- True or False
```

위의 예시 코드에서는 ```my_directory```라는 디렉토리의 경로를 ```directoryPath``` 변수에 할당하고, 이를 ```exists```함수의 매개변수로 전달합니다. 함수가 반환하는 값을 통해 디렉토리의 존재 여부를 파악할 수 있습니다.

## 깊이 있는 설명

Elm의 ```Filesystem.exists``` 함수는 지정된 디렉토리가 코드가 실행되는 위치에 존재하는지만을 확인합니다. 따라서 상대 경로가 아닌 절대 경로를 사용하는 것이 좋습니다. 또한, 해당 함수는 디렉토리의 존재 여부만을 확인하기 때문에 별도로 디렉토리를 만들어주는 함수가 필요합니다. 이를 위해 ```Directory.create``` 함수를 사용할 수 있으며, 이 함수는 디렉토리가 존재하지 않을 경우 해당 경로에 디렉토리를 생성합니다.

## 참고 자료

- [Elm Filesystem Documentation](https://package.elm-lang.org/packages/elm/filesystem/latest/)
- [Elm Filesystem.exists Documentation](https://package.elm-lang.org/packages/elm/filesystem/latest/Filesystem#exists)
- [Elm Directory.create Documentation](https://package.elm-lang.org/packages/elm/filesystem/latest/Filesystem-Directory#create)