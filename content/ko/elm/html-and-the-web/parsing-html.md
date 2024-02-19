---
aliases:
- /ko/elm/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:12.425325-07:00
description: "Elm\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uB85C\uBD80\
  \uD130 \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\
  \uB2C8\uB2E4. \uC6F9 \uCF58\uD150\uCE20\uB098 HTML\uC744 \uBC18\uD658\uD558\uB294\
  \ API\uC640 \uC778\uD130\uD398\uC774\uC2A4\uD558\uAE30 \uC704\uD574 \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC774 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uBA74\
  , \uB354 \uC0C1\uD638\uC791\uC6A9\uC801\uC774\uACE0 \uB3D9\uC801\uC778 \uC6F9 \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158\uC744 \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
lastmod: 2024-02-18 23:09:06.082469
model: gpt-4-0125-preview
summary: "Elm\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uB85C\uBD80\uD130\
  \ \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uC6F9 \uCF58\uD150\uCE20\uB098 HTML\uC744 \uBC18\uD658\uD558\uB294 API\uC640\
  \ \uC778\uD130\uD398\uC774\uC2A4\uD558\uAE30 \uC704\uD574 \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC774 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uBA74, \uB354 \uC0C1\
  \uD638\uC791\uC6A9\uC801\uC774\uACE0 \uB3D9\uC801\uC778 \uC6F9 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC744 \uB9CC\uB4E4 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Elm에서 HTML 파싱은 HTML 문서로부터 정보를 추출하는 것을 의미합니다. 웹 콘텐츠나 HTML을 반환하는 API와 인터페이스하기 위해 프로그래머들이 이 작업을 수행하면, 더 상호작용적이고 동적인 웹 애플리케이션을 만들 수 있습니다.

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
