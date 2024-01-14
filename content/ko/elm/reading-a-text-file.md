---
title:    "Elm: 텍스트 파일 읽기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 데 관심을 가지게 되는 이유는 많지만, 그 중 하나는 데이터를 처리하거나 분석하거나 사용하기 위해서입니다. 

## 방법

텍스트 파일을 읽는 것은 Elm의 `File` 모듈을 사용하여 쉽게 할 수 있습니다. 먼저, 파일 데이터를 저장할 변수를 선언하고 파일을 열 때 사용될 `File.reader` 함수를 정의합니다. 그런 다음 `File.read` 함수를 사용하여 파일 데이터를 읽은 다음, `Result` 타입을 사용하여 실패나 성공 여부를 처리합니다. 아래는 파일을 읽는 간단한 예제 코드입니다:

```Elm
-- 파일 데이터를 저장하기 위한 변수 선언
fileData : String

-- 파일을 열 때 사용될 `File.reader` 함수 정의
reader : File.Reader
reader =
    { onNextChar = \char -> ( fileData = fileData ++ toString char, ( reader ), ( reader ) )
    , onEndOfFile = \_ -> ( fileData, ( reader ), ( reader ) )
    , onError = \error -> ( fileData, ( reader ), ( reader ) ) 
    }

-- 파일 데이터를 읽음
File.read "text.txt" reader

```

위 코드는 `text.txt` 파일을 읽을 때 사용되며, 파일 데이터는 `fileData` 변수에 저장됩니다. 

## 딥 다이브

`File.reader` 함수의 콜백 함수들 중에서 `onNextChar` 함수는 매개 변수로 전달되는 문자 하나하나를 읽을 수 있습니다. 이를 활용하면 파일 데이터를 읽을 때 추가적인 로직을 적용할 수 있습니다. 또한, `File` 모듈에는 옵션으로 파일을 읽을 때 인코딩 방식을 지정할 수 있는 기능도 있습니다.

## 참고

- [Elm `File` 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/File)
- [Elm과 파일 다루기](https://programmableweb.com)
- [표준 입출력 장치와 파일 입출력](http://talestostories.tistory.com)