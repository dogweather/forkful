---
title:                "임시 파일 만들기"
html_title:           "Elm: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 
당신이 임시 파일을 만드는 것에 참여해야하는 이유는 무엇일까요? 임시 파일은 프로그래머들이 개발과 디버그를 효과적으로 수행할 수 있도록 도와줍니다.

# 방법 
우선, 임시 파일을 만드는 방법은 매우 간단합니다. 우리는 Elm의 기본 라이브러리인 `File` 모듈을 사용하여 스트링으로 된 임시 파일명을 생성할 수 있습니다. 아래 코드 예제를 살펴봅시다.

```Elm
import File
import Random

-- 임시 파일명 생성
createTempFile = 
  let
    tempName = "/tmp/file_{Random.int 10000}"
  in
    tempName
```

위 코드에서 우리는 `File` 모듈과 `Random` 모듈을 불러온 후, `Random` 모듈을 사용하여 무작위로 생성된 숫자를 `tempName`에 덧붙여 임시 파일명을 생성합니다.

이제 이 임시 파일명을 사용하여 실제 파일을 생성할 수 있습니다. 아래 코드 예제를 살펴봅시다.

```Elm
import File
import Random

-- 임시 파일 생성
createTempFile = 
  let
    tempName = "/tmp/file_{Random.int 10000}"
  in
    tempName

-- 임시 파일 생성 후 데이터 추가
appendToFile =
  let
    tempFileName = createTempFile
  in
    File.write tempFileName "This is a temporary file."
```

위 코드에서 우리는 `createTempFile` 함수를 사용하여 임시 파일명을 생성한 뒤, 이 이름을 `File.write` 함수의 인자로 사용하여 파일에 데이터를 추가합니다.

# 딥 다이브 
위에서는 간단한 방법으로 임시 파일을 생성하고 데이터를 추가하는 방법을 살펴봤습니다. 그러나 실제로는 이보다 더 많은 기능을 지원하는 `File` 모듈이 존재합니다. 예를 들어, `File` 모듈은 파일의 존재 여부를 확인하거나 삭제하는 기능도 제공합니다. 더 많은 정보를 원한다면, Elm 공식 문서에서 `File` 모듈을 더 자세히 살펴보는 것을 추천합니다.

# 함께 보기 
- [Elm 공식 문서 - File 모듈](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm 공식 문서 - Random 모듈](https://package.elm-lang.org/packages/elm/random/latest/)
- [다양한 예제를 담은 Elm 샘플 레파지토리](https://github.com/evancz/elm-samples)