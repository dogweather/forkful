---
aliases:
- /ko/elm/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:30.618072-07:00
description: "Elm\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB300\
  \uD45C\uD558\uB294 \uD14D\uC2A4\uD2B8 \uC815\uBCF4\uB97C Elm\uC774 \uC774\uD574\uD558\
  \uACE0 \uC870\uC791\uD560 \uC218 \uC788\uB294 \uD615\uC2DD, \uAD6C\uCCB4\uC801\uC73C\
  \uB85C\uB294 `Date` \uD0C0\uC785\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\uC815\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740 \uC0AC\uC6A9\uC790 \uC785\
  \uB825\uC744 \uCC98\uB9AC\uD558\uACE0, \uC9C0\uC5ED\uD654\uB41C \uB0A0\uC9DC\uB97C\
  \ \uC62C\uBC14\uB974\uAC8C \uD45C\uC2DC\uD558\uBA70, \uB0A0\uC9DC \uAD00\uB828 \uACC4\
  \uC0B0\uC744\u2026"
lastmod: 2024-02-18 23:09:06.100729
model: gpt-4-0125-preview
summary: "Elm\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB300\
  \uD45C\uD558\uB294 \uD14D\uC2A4\uD2B8 \uC815\uBCF4\uB97C Elm\uC774 \uC774\uD574\uD558\
  \uACE0 \uC870\uC791\uD560 \uC218 \uC788\uB294 \uD615\uC2DD, \uAD6C\uCCB4\uC801\uC73C\
  \uB85C\uB294 `Date` \uD0C0\uC785\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uACFC\uC815\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740 \uC0AC\uC6A9\uC790 \uC785\
  \uB825\uC744 \uCC98\uB9AC\uD558\uACE0, \uC9C0\uC5ED\uD654\uB41C \uB0A0\uC9DC\uB97C\
  \ \uC62C\uBC14\uB974\uAC8C \uD45C\uC2DC\uD558\uBA70, \uB0A0\uC9DC \uAD00\uB828 \uACC4\
  \uC0B0\uC744\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜 하나요?
Elm에서 문자열로부터 날짜를 파싱하는 것은 날짜와 시간을 대표하는 텍스트 정보를 Elm이 이해하고 조작할 수 있는 형식, 구체적으로는 `Date` 타입으로 변환하는 과정을 말합니다. 이 과정은 사용자 입력을 처리하고, 지역화된 날짜를 올바르게 표시하며, 날짜 관련 계산을 수행함으로써, Elm 애플리케이션이 시간 데이터를 지능적으로 처리할 수 있도록 하는 데 필수적입니다.

## 어떻게 하나요:
Elm은 일부 다른 언어들처럼 날짜 파싱에 강력한 내장 기능을 가지고 있지 않으며 주로 Javascript 상호 운용성 또는 더 복잡한 작업을 위한 라이브러리에 의존합니다. 하지만, 기본 파싱을 위해 `elm/time` 패키지를 사용할 수 있고, 보다 복잡한 요구사항에는 제3자 `justinmimbs/date` 라이브러리 사용이 널리 추천됩니다.

### `elm/time`을 사용한 파싱:
`elm/time`은 `Time` 모듈을 제공하여, 사람이 읽을 수 있는 날짜 대신 타임스탬프로 작업할 수 있게 합니다. 이는 문자열로부터 직접 날짜를 파싱하지는 않지만, ISO 8601 문자열을 POSIX 타임스탬프로 변환할 수 있으며, 이를 통해 작업할 수 있습니다.

```elm
import Time exposing (Posix)

-- ISO 8601 날짜 문자열을 가지고 있다고 가정
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- POSIX 타임스탬프로 변환하기 (이 함수는 `Result`를 반환합니다)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- 샘플 출력: Ok <posix 시간 값>
```

### `justinmimbs/date` 사용한 파싱:
비ISO 형식 등 보다 복잡한 파싱 작업에는 `justinmimbs/date` 라이브러리가 좋은 선택입니다. 다음은 사용자 정의 날짜 문자열을 파싱하는 방법입니다:

1. 라이브러리가 설치되어 있는지 확인하세요:

```shell
elm install justinmimbs/date
```

2. 사용자 정의 날짜 형식을 파싱하기 위해 `Date.fromString` 함수 사용하기:

```elm
import Date
import Result exposing (Result(..))

-- 사용자 정의 날짜 문자열 형식 `dd-MM-yyyy`를 가정
customDateStr : String
customDateStr = "01-01-2023"

-- 사용자 정의 형식을 파싱하는 함수
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- 샘플 사용
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- 샘플 출력: Ok (Date.fromCalendarDate 2023 Jan 1)
```

이 예시에서, `Result` 타입은 성공적인 파싱으로 날짜(`Ok`)를 산출하거나 오류(`Err`)를 감싸지므로, Elm 애플리케이션에서 강력한 오류 처리를 가능하게 합니다.
