---
title:    "Elm: 두 날짜 비교하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

Elm은 대부분의 프로그래밍 언어와 달리 타입 시스템과 정적 결정을 가지고 있습니다. 이것은 Elm이 더 안전하고 예측 가능한 프로그래밍을 할 수 있도록 도와줍니다. 그 중 하나가 두 개의 날짜를 비교하는 것입니다. 왜 두 개의 날짜를 비교해야 할까요? 이를 알아봅시다!

## 어떻게 비교할까요?

아래의 코드 블록을 보면, 두 개의 날짜를 비교하는 방법을 알 수 있습니다. 먼저 ```Date.fromString``` 함수를 사용하여 날짜를 문자열에서 변환하고, 다음으로, ```Html.text``` 함수를 사용하여 결과를 화면에 보여줍니다. 결과를 보면, 이전 날짜가 최신 날짜보다 이전이라는 것을 알 수 있습니다. 코드를 실행하면서 다른 날짜들을 비교해보세요!

```elm
Date.fromString "2020-01-01" |> Date.fromString "2021-01-01"
|> Date.compare
|> Html.text
```

**코드 출력:**
```
LT
```

## 깊게 파보자

Elm에서 두 개의 날짜를 비교하는 더 많은 방법들이 있습니다. 예를 들어, ```Date.Compare``` 모듈에는 3개의 함수가 있습니다: ```Date.before```, ```Date.after```, 그리고 ```Date.between```. 각각 다른 비교 방법을 제공합니다. 또한, 날짜를 비교할 뿐만 아니라, 날짜가 유효한지 여부를 확인할 수도 있습니다. 이를 위해서는 ```Date.isValid``` 함수를 사용하면 됩니다. 더 많은 정보를 원한다면, Elm 공식 문서의 [날짜와 시간](https://guide.elm-lang.org/interop/dates.html) 부분을 참고해보세요.

## 관련 자료

- [Elm 공식 문서 - 날짜와 시간](https://guide.elm-lang.org/interop/dates.html)
- [Elm 정적 타입 시스템에 대한 소개 (번역)](https://jjminsik.tistory.com/entry/Introduction-to-Elm-typing)
- [JavaScript와의 상호 운용성을 위한 Elm 마법사 삽질기 (번역)](https://riiid-techblog.tistory.com/32)