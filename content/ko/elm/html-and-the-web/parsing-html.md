---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:12.425325-07:00
description: "\uBC29\uBC95: Elm\uC740 \uD0C0\uC785 \uC548\uC815\uC131\uACFC \uB7F0\
  \uD0C0\uC784 \uC624\uB958\uB97C \uD53C\uD558\uB294 \uAC83\uC5D0 \uC911\uC810\uC744\
  \ \uB450\uAE30 \uB54C\uBB38\uC5D0, JavaScript\uB098 Python\uC758 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uC640 \uAC19\uC774 \uC9C1\uC811 HTML\uC744 \uD30C\uC2F1\uD558\uAE30\
  \ \uC704\uD55C \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC5C6\uC2B5\uB2C8\
  \uB2E4. \uD558\uC9C0\uB9CC, `Http` \uC694\uCCAD\uC744 \uC0AC\uC6A9\uD558\uC5EC \uCF58\
  \uD150\uCE20\uB97C \uAC00\uC838\uC628 \uB2E4\uC74C \uC815\uADDC \uD45C\uD604\uC2DD\
  \uC774\uB098 \uC11C\uBC84 \uCE21\u2026"
lastmod: '2024-03-13T22:44:55.107879-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uD0C0\uC785 \uC548\uC815\uC131\uACFC \uB7F0\uD0C0\uC784 \uC624\
  \uB958\uB97C \uD53C\uD558\uB294 \uAC83\uC5D0 \uC911\uC810\uC744 \uB450\uAE30 \uB54C\
  \uBB38\uC5D0, JavaScript\uB098 Python\uC758 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC640\
  \ \uAC19\uC774 \uC9C1\uC811 HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD55C \uB0B4\
  \uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Elm은 타입 안정성과 런타임 오류를 피하는 것에 중점을 두기 때문에, JavaScript나 Python의 라이브러리와 같이 직접 HTML을 파싱하기 위한 내장 라이브러리가 없습니다. 하지만, `Http` 요청을 사용하여 콘텐츠를 가져온 다음 정규 표현식이나 서버 측 처리를 사용해 필요한 정보를 추출할 수 있습니다. 더 복잡한 HTML 파싱을 위해 일반적으로 사용되는 방법은 전용 백엔드 서비스를 사용하여 HTML을 파싱하고 Elm이 직접 작업할 수 있는 형식, 예를 들어 JSON으로 데이터를 반환하는 것입니다.

HTML 콘텐츠를 가져오는 예시입니다(서버 응답이 깨끗한 형식이거나 특정 태그 콘텐츠인 경우를 가정합니다):

```elm
import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

initialModel : Model
initialModel =
    { content = "" }

type Msg
    = Fetch
    | ReceiveContent String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com"
                , expect = Http.expectString ReceiveContent
                }
            )

        ReceiveContent content ->
            ( { model | content = content }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    text model.content

-- 메인 함수와 구독 정의는 Elm의 표준 애플리케이션 구조를 따른다고 가정하십시오.
```

응답을 처리하여 특정 요소나 데이터를 실제로 파싱하려면, 제어하는 서버 엔드포인트로 HTML 콘텐츠를 전송하여 JavaScript(Cheerio, Jsdom) 또는 Python(BeautifulSoup, lxml)과 같은 언어에서 사용 가능한 라이브러리로 파싱한 다음, 구조화된 데이터(예: JSON)를 Elm 애플리케이션으로 다시 반환하는 것을 고려해 볼 수 있습니다.

클라이언트 측 Elm 코드에서 직접 HTML을 파싱하는 것은 언어 제약과 콘텐츠 가져오기와 콘텐츠 처리 사이의 명확한 분리를 장려하는 철학 때문에 일반적인 패턴이 아님을 기억하세요. Elm 아키텍처는 JSON과 같은 더 안전하고 예측 가능한 형식으로 데이터를 처리하는 쪽으로 기울어져 있습니다.
