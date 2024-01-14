---
title:                "Elm: 텍스트 파일 읽기"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

이번에는 Elm에서 텍스트 파일을 읽는 방법에 대해 알아보고자 합니다. 텍스트 파일은 데이터를 저장하고 공유하는데 유용합니다. 이 글을 통해 Elm에서 텍스트 파일을 읽는 방법을 쉽게 익힐 수 있습니다.

# 어떻게

Elm에서 텍스트 파일을 읽는 방법은 간단합니다. 먼저, `elm/file` 라이브러리를 사용하여 파일을 가져와야 합니다. 그리고 `Text` 모듈에서 `lines` 함수를 사용하여 파일을 줄 단위로 읽어올 수 있습니다. 이렇게 읽어온 데이터는 `List String` 형태로 저장됩니다. 예제 코드를 통해 살펴보도록 하겠습니다.

```Elm
import File
import Text exposing (lines)

file : File
file =
    File.read "example.txt"

readFile : File.Result String
readFile =
    File.readAsString file

parsedLines : List String
parsedLines =
    readFile
        |> Result.map (lines FileSystem.Readable.utf8)
        |> Result.withDefault []
```

위 예제 코드에서 `example.txt` 파일을 읽고 `lines` 함수를 사용하여 데이터를 줄 단위로 분리해 `List String` 형태로 저장합니다. 만약 파일을 읽지 못할 경우 기본값인 빈 리스트가 반환됩니다. 함수를 사용해 텍스트 파일 내용을 단순하게 가져올 수 있습니다.

# 딥 다이브

이제 텍스트 파일을 간단하게 읽는 방법에 대해 알아보았습니다. 하지만 더 깊숙한 수준에서 파일을 읽을 수 있도록 하는 다양한 함수들도 존재합니다. 예를 들어 `File.readAsString` 함수를 사용하면 파일을 읽어 `String` 형태로 저장할 수 있으며, `File.readAsLines` 함수를 사용하면 파일을 읽어 `List (List String)` 형태로 저장할 수 있습니다.

# 관련 링크

- [Elm 공식 문서 - 파일 읽기](https://guide.elm-lang.org/io/files.html)
- [Elm 공식 문서 - Text 모듈](https://package.elm-lang.org/packages/elm-lang/core/latest/Text)
- [Elm Guide - 파일 다루기](https://guide.elm-lang.org/effects/file.html)