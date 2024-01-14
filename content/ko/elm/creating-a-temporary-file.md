---
title:    "Elm: 임시 파일 만들기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜?
왜 임시 파일을 생성하는 것에 참여해야 할까요? 여러분은 여러분의 프로그램에서 일시적으로 데이터를 저장해야 할 때 임시 파일이 유용할 수 있습니다. 예를 들어 큰 데이터를 처리하는 중에 중간 결과를 저장하거나, 다른 애플리케이션과 데이터를 공유하려는 경우에 유용합니다.

## 어떻게?
다음은 Elm을 사용하여 임시 파일을 생성하는 간단한 예제 코드입니다:
```Elm
import File

createTempFile : String -> Cmd msg
createTempFile content =
  File.create "temp.txt" content

main : Program () Model Msg
main =
  init ()
    |> Task.perform (always Cmd.none) (createTempFile "Temporary data")
```
`createTempFile` 함수는 `temp.txt` 파일을 생성하고 `content`를 파일에 씁니다. 이제 우리는 임시 파일을 사용할 준비가 되었습니다!

## 깊이 파고들기
임시 파일을 생성할 때 주의해야 할 몇 가지 사항이 있습니다. 첫째, 임시 파일이 생성되는 위치를 지정하는 것이 중요합니다. Elm은 브라우저에서 실행되므로, 우리는 브라우저의 제한된 파일 시스템 내에만 임시 파일을 생성할 수 있습니다. 또한 임시 파일은 프로그램이 종료되면 자동으로 삭제됩니다. 따라서 우리는 임시 파일이 아닌 다른 파일에 저장할 데이터는 사용하기 적절하지 않습니다.

## 또 다른 자료
- [Elm 공식 문서 - 파일 다루기](https://guide.elm-lang.org/interop/file.html)
- [링크: 파일 API에 대한 MDN 문서](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [링크: 브라우저의 파일 시스템에 대한 MDN 문서](https://developer.mozilla.org/en-US/docs/Web/API/FileSystem)
- [링크: Elm 프로그래밍 언어 공식 페이지](https://elm-lang.org/)