---
title:                "Elm: 임시 파일 만들기"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만드는데 참여하는 이유는 무엇일까요? Elm을 사용하면서 임시 파일을 만드는 경우가 있을 수 있습니다. 이번 포스트에서는 이러한 경우들을 알아보고 임시 파일을 어떻게 만들 수 있는지 배워보겠습니다.

## 어떻게

우선, 임시 파일을 만들기 위해 다음과 같은 코드를 작성해야 합니다.

```Elm
import File
import Random

genRandName : Int -> String
genRandName num =
    Random.generate (Random.int 0 999) 
        |> Task.perform (\val -> 
            "temp_file_" ++ (String.padLeft 3 '0' (toString val)) ++ ".txt"
        )
        
makeTempFile : Int -> Cmd msg
makeTempFile num =
    Task.perform (\fname -> 
        (fmae, File.write "/path/to/directory/" fname "This is a temporary file!")
    (genRandName num)

```

이제, 이 코드를 실행하면 지정한 경로에 임시 파일이 생성될 것입니다. 이 파일은 "temp_file_###.txt" 같은 형식의 이름을 가지며, 0에서 999 사이의 임의의 숫자가 들어갑니다. 파일에는 "This is a temporary file!"라는 내용이 들어 있습니다.

## 딥 다이브

임시 파일을 만드는 프로세스는 간단하면서도 유용합니다. Elm의 Random 모듈을 사용하여 임의의 숫자를 생성하고, Task.perform 함수를 통해 "temp_file_###.txt"와 같은 이름을 만들어 주는 것입니다. 이러한 방식으로 임시 파일을 만들어서 다양한 용도로 사용할 수 있습니다.

## 참고 자료

- [Elm 공식 문서 - File](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Elm 공식 문서 - Random](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Elm 프로젝트에서 임시 파일 생성하기](https://dev.to/davembush/creating-temporary-files-in-elm-2mdk)