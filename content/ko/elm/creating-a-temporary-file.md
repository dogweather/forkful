---
title:    "Elm: 임시 파일 생성하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 왜?
당신은 개발을 할 때 임시 파일을 만드는 일에 관심이 있을 수 있습니다. 이는 여러분이 프로그램을 디버깅하거나 중간 결과를 저장하는 데에 도움이 되기 때문입니다.

## 하는 법
임시 파일을 생성하는 것은 매우 간단합니다. 우리는 `File.withTempFile` 함수를 사용할 것입니다. 이 함수는 두 가지 인수를 받습니다. 첫 번째 인수는 임시 파일의 이름을 지정하는데 사용될 문자열로서 일반적으로 `".tmp"`와 같이 확장자를 포함하게 될 것입니다. 두 번째 인수는 파일을 다루기 위한 콜백 함수입니다. 이 콜백 함수는 임시 파일에 대해 액션을 수행합니다.

``` Elm
import File

File.withTempFile ".tmp" (\tempFile ->
    -- 여기에서 임시 파일에 대한 액션을 수행합니다.
    -- 예를 들어, 파일에 내용을 쓰거나 읽을 수 있습니다.
)
```

위의 예제에서, 우리는 `withTempFile` 함수를 사용하여 새로운 임시 파일의 경로를 지정하고 그 파일을 다루기 위한 콜백 함수를 정의하였습니다.

## 깊게 파고들기
우리는 `withTempFile` 함수를 사용하는 대신에 `File.TempFile` 모듈을 통해 임시 파일을 만들 수도 있습니다. 이 모듈은 더 많은 유연성을 제공합니다. 예를 들면, 우리는 `File.TempFile.with` 함수를 사용하여 임시 파일을 생성할 때 사용될 디렉토리를 지정할 수도 있습니다. 또한, 우리는 `File.TempFile.cleanup` 함수를 사용하여 생성된 임시 파일들을 정리할 수 있습니다.

``` Elm
import File.TempFile as TempFile

TempFile.with ".tmp" (\tempFile ->
    -- 여기에서 임시 파일에 대한 액션을 수행합니다.
    -- 예를 들어, 파일에 내용을 쓰거나 읽을 수 있습니다.
) |> TempFile.cleanup
```

## 더 알아보기
임시 파일을 만드는 것 외에도, `File.TempFile` 모듈은 임시 파일 및 디렉토리를 적절히 관리하고 처리하기 위한 다양한 함수들을 제공합니다. 더 많은 정보를 알아보고 싶다면 [공식 문서](https://package.elm-lang.org/packages/elm/file/latest/File-TempFile)를 참고해주세요.

# 참고자료
- [withTempFile 함수 공식 문서](https://package.elm-lang.org/packages/elm/file/latest/File#withTempFile)
- [File.TempFile 모듈 공식 문서](https://package.elm-lang.org/packages/elm/file/latest/File-TempFile)