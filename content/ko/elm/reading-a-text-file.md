---
title:                "Elm: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 고민할 필요가 있습니다. 모든 프로그램은 데이터를 처리하고 이를 이용해 결과물을 만들기 때문이죠. 텍스트 파일을 읽는 것은 많은 장점을 가지고 있습니다. 가령, 이전에 접한적 없는 데이터셋에 대해 통찰력을 얻을 수 있고, 다른 애플리케이션에서 생성한 데이터를 읽고 병합할 수도 있습니다.

## 어떻게

우선 다음 코드와 같이 `File` 모듈을 가져옵니다.

```Elm
import File exposing (readFile)
```

그리고 `readFile` 함수를 통해 텍스트 파일의 경로를 전달하고 `Result`를 반환하도록 합니다. 이를 통해 파일을 읽고 쓸 수 있는 권한을 얻을 수 있습니다.

```Elm
readFile : String -> Task x (Result x String)
```

마지막으로, `readFile` 함수의 콜백함수를 통해 파일의 내용을 읽을 수 있고, 이를 활용하여 필요한 작업을 수행할 수 있습니다.

```Elm
readFile path
    |> Task.perform
        (\_ -> []
        )
        (\result ->
            case result of
                Err err ->
                    Debug.log "Error reading file" err

                Ok contents ->
                    -- 파일 내용을 활용하여 필요한 작업 수행
        )
```

## 깊이 파고들기

하기 링크들을 참조하시면 `File` 모듈의 더 구체적인 사용법을 알 수 있습니다.

- [Elm 공식 문서](https://guide.elm-lang.org/io/files.html)
- [괴팍한 존재의 블로그](http://www.gpfault.org/blog/2016/06/30/elm%EC%97%90%EC%84%9C-%ED%85%8D%EC%8A%A4%ED%8A%B8-%ED%8C%8C%EC%9D%BC-%EC%9D%BD%EA%B8%B0/)
- [진일모의 블로그](https://jinhyewon.github.io/elm/file-io/) 

## 또 다른 참고 자료

- [Elm 설치 가이드](https://dogfeet.github.io/articles/2018/elm01)
- [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)
- [Elm 한국 사용자 그룹 Facebook 페이지](https://www.facebook.com/groups/elminkorea/)