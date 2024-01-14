---
title:                "Elm: 디렉토리가 존재하는지 확인하는 방법"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

Elm 프로그래밍을 배우는 동안, 당신은 아마도 '디렉토리가 존재하는지 확인하는 방법'이 이유가 궁금해 지셨을 것입니다. 쉽게 말해서, 디렉토리가 존재하는지 확인하는 것은 파일이나 데이터를 저장하는데 사용하는 디렉토리가 실제로 존재하는지 알아보기 위해서입니다.

## 어떻게 할까요

먼저, `elm/file` 모듈을 가져옵니다. 다음으로, `File.exists` 함수를 사용하여 디렉토리의 존재 여부를 확인합니다. 코드는 다음과 같습니다:
```
import File

directoryPath : String
directoryPath = "data"

File.exists directoryPath
```
위의 코드를 실행하면 "True" 또는 "False" 값을 얻을 수 있습니다. True는 디렉토리가 존재하는 것을 의미하고, False는 디렉토리가 존재하지 않는 것을 의미합니다.

## 깊게 파보기

`File.exists` 함수는 실제적으로 디렉토리가 존재하는지 확인하기 위해 시스템 호출을 수행합니다. 이 함수는 비동기적으로 실행되기 때문에, 결과를 얻기 위해서는 콜백 함수를 사용해야 합니다.

파일 시스템에 대한 좀 더 자세한 정보를 알고 싶다면, `elm/virtual-dom` 모듈을 살펴보십시오. 또한 Elm에는 Elm 파일 시스템 모듈 이외에도 다른 방법을 제공하기 때문에, 더 많은 선택권을 가지고 싶다면, 더 많은 리서치를 해보십시오.

## 연관된 자료

* [File system in Elm](https://guide.elm-lang.org/architecture/effects/filesystem.html)
* [Files and directories in Elm](https://package.elm-lang.org/packages/elm/file/latest/)
* [Using Virtual DOM in Elm](https://package.elm-lang.org/packages/elm/virtual-dom/latest/)