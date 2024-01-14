---
title:    "Elm: 디렉터리 존재 여부 확인하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜 디렉토리가 존재하는지 체크해야 하는가

 Elm은 안전한 타입 시스템과 함수형 프로그래밍의 장점을 결합하여, 신뢰할 수 있고 유지보수가 용이한 코드 작성을 도와줍니다. 따라서, Elm을 사용하는 개발자들은 디렉토리가 존재하는지 여부를 체크하는 것을 통해 코드의 안정성과 견고성을 높일 수 있습니다.

# 디렉토리가 존재하는지 체크하는 방법

```elm
import File

checkDirectory : String -> Cmd msg
checkDirectory path =
    File.isDirectory path
        |> Task.andThen
            ( \isDirectory ->
                if isDirectory then
                    -- 디렉토리가 존재한다면 원하는 작업을 수행합니다.
                    performSomeTask
                        |> Task.toCmd
                else
                    -- 디렉토리가 존재하지 않는다면 에러를 띄우고 실패를 알려줍니다.
                    Task.fail "해당 디렉토리가 존재하지 않습니다."
                        |> Task.toCmd
            )
```

위 예시 코드는 `File.isDirectory` 함수를 사용하여 원하는 디렉토리가 존재하는지 확인하고, 그에 따라 적절한 작업을 수행하는 방법을 보여줍니다.

# 깊이있게 알아보기

`File.isDirectory` 함수는 어떻게 작동하고, 디렉토리를 찾는 데 어떤 방법을 사용하는지 자세히 살펴보겠습니다.

먼저, `File.isDirectory` 함수는 주어진 경로(`path`)가 디렉토리인지 확인하는 `Task`를 반환합니다. 이를 통해, 디렉토리의 존재 여부를 확인할 수 있습니다. 따라서 이 함수를 사용할 때는 `Task.andThen` 함수를 함께 사용하여, 존재하지 않는 디렉토리를 체크할 때 발생할 수 있는 에러를 적절한 방식으로 처리할 수 있도록 해야 합니다.

# See Also

- [Elm 공식 문서 - File](https://package.elm-lang.org/packages/elm/file/latest/File#isDirectory)
- [Elm 공식 문서 - Task](https://package.elm-lang.org/packages/elm/core/latest/Task)
- [Elm으로 디렉토리 내부의 파일 목록 가져오기(영문)](https://medium.com/@praveenweb/elm-loop-through-file-name-873c25843459)