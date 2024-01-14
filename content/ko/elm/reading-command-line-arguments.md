---
title:    "Elm: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜
커맨드 라인 인자를 읽는 것을 읽습니다.

## 어떻게
커맨드 라인 인자를 읽는 방법을 살펴보겠습니다. Elm에서는 `elm/core` 패키지의 `Platform.Cmd` 모듈을 통해 커맨드 라인 인자를 읽을 수 있습니다. 예를 들어, 다음 코드를 실행하면 사용자가 입력한 인자를 읽어와 출력할 수 있습니다.

```elm
import Platform.Cmd as Cmd
import Console exposing (log)

main =
    Cmd.args
        |> Cmd.map (\args -> log (List.head args))
        |> Cmd.perform
```

위의 코드를 실행한 뒤 `elm reactor` 커맨드로 브라우저에서 확인해보면, 입력한 인자가 출력되는 것을 확인할 수 있습니다.

```
elm reactor --port=8000
8000
```

더 많은 커맨드 라인 인자를 읽어오는 방법은 `elm/core` 패키지의 `Platform.Cmd` 모듈 문서를 참고하시기 바랍니다.

## 깊게 파고들기
커맨드 라인 인자를 읽는 것은 부하에 따라 존재하지 않을 수도 있습니다. 이는 웹 브라우저에서는 일반적으로 사용되지 않고, 대신 네이티브 컴파일러에서 주로 사용됩니다. 네이티브 모듈에서는 `elm/core` 패키지의 `Platform.Cmd` 모듈 대신 `Elm.Kernel.Cmd` 모듈을 사용하여 커맨드 라인 인자를 읽습니다.

## 관련 링크
- [Platform.Cmd 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Elm.Kernel.Cmd 모듈 코드](https://github.com/elm/kernel/blob/8f19754/cmd/src/Elm/Kernel/Cmd.js) (네이티브 모듈)