---
title:                "Elm: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-json.md"
---

{{< edit_this_page >}}

# 왜

JSON 데이터는 현대 웹 애플리케이션에서 필수적입니다. 따라서 JSON을 탐색하고 사용하기 위해 더 많은 사람들이 Elm 프로그래밍 언어를 배우고 있습니다. 여기서는 Elm에서 JSON 데이터를 다루는 방법에 대해 알아보겠습니다.

## 어떻게

JSON 데이터를 다루기 위해서는 해당 데이터를 엽니다. 그리고 이 데이터를 정확하게 나타내는 타입이 필요합니다. Elm에서는 `Decode`라는 타입을 사용하여 JSON 데이터를 디코딩합니다. 아래 코드 블록을 살펴보세요.

```Elm
Decode.decodeString (Decode.list Decode.string) """["Elm", "프로그래밍", "언어"]```

이 코드는 `"["Elm", "프로그래밍", "언어"]`라는 문자열을 리스트로 디코딩하고, 각 원소를 문자열로 디코딩하는 예제입니다. 디코딩된 결과를 보면 `Result` 타입으로 표현됩니다. 이를 패턴 매칭을 사용하여 각각의 경우에 맞게 처리할 수 있습니다.

아래는 결과를 출력하는 코드 블록 예제입니다.

```Elm
case Decode.decodeString (Decode.list Decode.string) """["Elm", "프로그래밍", "언어"]``` 
of
Result.Ok result -> 
    Debug.log "리스트 결과" result
Result.Err error -> 
    Debug.log "에러" error
```

위 코드를 실행하면 아래와 같은 결과를 볼 수 있습니다.

```
{ intendedStructure = List [ "Elm", "프로그래밍", "언어" ] }
```

## 깊게 파헤치기

더 복잡한 JSON 데이터를 다루기 위해서는 더 복잡한 타입을 정의하여 사용해야합니다. 이를 위해 Elm에서는 `Decode.field` 함수를 제공합니다.

아래 예제 코드는 간단한 JSON 데이터를 디코딩하고, 특정 필드의 값을 가져와 출력하는 예제입니다.

```Elm
Decode.field "name" Decode.string
    |> Decode.decodeString """{ "name": "Elm" }"""
```

실행한 결과는 아래와 같습니다.

```
{ intendedStructure = "Elm" }
```

더 복잡한 경우에는 `field` 함수를 이용해서 더 복잡한 구조를 다룰 수 있습니다.

## 참고

- [Elm 공식 문서 - Decoding JSON](https://guide.elm-lang.org/effects/json.html)
- [Elm 공식 문서 - Result 타입](https://package.elm-lang.org/packages/elm/core/latest/Result)
- [JSON의 구조에 대한 더 자세한 설명](https://www.json.org/json-ko.html)