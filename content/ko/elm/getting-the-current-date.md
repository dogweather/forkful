---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
현재의 날짜를 얻는 것은 컴퓨터 시스템의 내장된 달력 데이터를 받아와서 현재 일시를 알려주는 것을 말한다. 이것은 사용자 인터페이스에 일시 정보를 제공하거나,  데이터의 시간적 관계를 파악하는 등 다양한 프로그래밍 작업에서 필요하다.

## 어떻게 해야하나요:
아래는 Elm 프로그래밍에서 현재 날짜를 얻는 방법에 대한 코드 예제입니다.

```elm
import Time exposing (Posix)

type Msg = 
    GetCurrentTime
    | ReceiveCurrentTime Posix

 GetCurrentTime ->
    Time.now |> Task.attempt ReceiveCurrentTime

ReceiveCurrentTime time ->
    ( { model | time = Just time }, Cmd.none )
```
위 코드를 실행하면 현재 시간을 포식스 (Posix)시간으로 리턴합니다.

## 깊이있게 알아보기:
Elm에서 현재 시간을 얻는 기능은 0.19 버전부터 사용할 수 있게 되었습니다. 그 전에는 Native 모듈을 통해 JavaScript 동작으로 이를 구현해야 했습니다.

다른 방법으로는 JavaScript와 Elm의 인터옵을 통해 JavaScript의 `Date()` 기능을 이용하여 현재 시간을 얻는 방법이 있습니다. 하지만 이 방법은 JavaScript와의 의존성을 높이는 단점이 있습니다.

내부 구현에서, Elm은 시스템 내부의 실제 시간을 가져오기 위해 POSIX 시간을 사용합니다. 1970년 1월 1일 00:00:00 UTC를 시작점으로 하는, 초 단위로 측정된 시간을 나타냅니다.

## 참고자료:
다음은 이 주제와 관련된 몇 가지 추가 리소스입니다.

- Elm 의 공식 [Time](https://package.elm-lang.org/packages/elm/time/latest/) 패키지 문서: Elm의 시간처리에 대한 자세한 정보를 제공합니다.
- Unix 및 POSIX 시간에 대한 [위키백과](https://ko.wikipedia.org/wiki/유닉스_시간) 설명: POSIX 시간의 기원과 그 의미에 대한 상세한 설명이 있습니다.