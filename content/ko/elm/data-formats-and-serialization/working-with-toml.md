---
date: 2024-01-26 04:21:28.530501-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C\
  , \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uC785\uB2C8\uB2E4. Elm \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\
  \uC11C \uD544\uC694\uD55C \uD0A4-\uAC12 \uC30D\uC5D0 \uAE54\uB054\uD558\uAC8C \uB9E4\
  \uD551\uB418\uACE0 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uC26C\uC6B4 \uC774\uC720\uB85C\
  \ \uAD6C\uC131 \uB370\uC774\uD130\uB97C \uAD00\uB9AC\uD558\uAE30 \uC704\uD574 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.143364-07:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C, \uB370\
  \uC774\uD130 \uC9C1\uB82C\uD654 \uC5B8\uC5B4\uC785\uB2C8\uB2E4. Elm \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C\
  \ \uD544\uC694\uD55C \uD0A4-\uAC12 \uC30D\uC5D0 \uAE54\uB054\uD558\uAC8C \uB9E4\uD551\
  \uB418\uACE0 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uC26C\uC6B4 \uC774\uC720\uB85C \uAD6C\
  \uC131 \uB370\uC774\uD130\uB97C \uAD00\uB9AC\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 약자로, 데이터 직렬화 언어입니다. Elm 프로그래머들은 응용 프로그램에서 필요한 키-값 쌍에 깔끔하게 매핑되고 사람이 읽기 쉬운 이유로 구성 데이터를 관리하기 위해 사용합니다.

## 방법:
Elm에는 내장 TOML 파서가 없지만, JavaScript와 상호 작용하거나 커뮤니티 패키지를 사용할 수 있습니다. 다음은 가상의 `elm-toml` 패키지를 사용하여 TOML을 구문 분석하는 방법입니다:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

특정 값 디코딩을 위해:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

`port`의 샘플 출력은 디코딩이 성공하면 `Ok 8080`일 수 있습니다.

## 심층 탐구
TOML은 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 생성되었으며, 설정 파일을 위한 간단한 언어로 됩니다. TOML은 YAML과 JSON과 경쟁하면서, 사람이 읽고 쓰기 쉬움을 목표로 하는 구문을 갖고 있습니다.

Elm에서 TOML을 다루려면 일반적으로 JavaScript 상호운용을 통해야 하며, 이는 조금 번거로울 수 있습니다. 다행히 Elm 커뮤니티는 자원이 풍부하여 여러 타사 패키지가 존재합니다. 가상의 `elm-toml` 패키지는 Elm의 `Port`를 사용하여 JavaScript TOML 파서와 대화하거나 Elm에서 직접 구문 분석을 구현할 가능성이 높습니다.

Elm에서의 주요 장애물은 모든 것을 정적으로 타입화하기 때문에, TOML 내 다양한 데이터 구조를 처리하기 위해 사용자 정의 디코더를 작성해야 하며, 이는 약간 장황할 수 있지만 안전성을 추가합니다.

## 참고
TOML 자체에 대한 사양 및 추가 정보는 [TOML](https://toml.io)를 확인하세요.
Elm과 JavaScript 상호운용에 대한 실용적인 접근 방식을 찾고 있다면, 공식 가이드인 [Elm Ports](https://guide.elm-lang.org/interop/ports.html)부터 시작하세요.
커뮤니티 패키지를 찾거나 기여하려면 [Elm Packages](https://package.elm-lang.org/)를 둘러보세요.
