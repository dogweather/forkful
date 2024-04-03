---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:30.618072-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Elm\uC740 \uC77C\uBD80 \uB2E4\
  \uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uB0A0\uC9DC \uD30C\uC2F1\uC5D0 \uAC15\uB825\
  \uD55C \uB0B4\uC7A5 \uAE30\uB2A5\uC744 \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC73C\
  \uBA70 \uC8FC\uB85C Javascript \uC0C1\uD638 \uC6B4\uC6A9\uC131 \uB610\uB294 \uB354\
  \ \uBCF5\uC7A1\uD55C \uC791\uC5C5\uC744 \uC704\uD55C \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, \uAE30\uBCF8 \uD30C\uC2F1\
  \uC744 \uC704\uD574 `elm/time` \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uACE0, \uBCF4\uB2E4 \uBCF5\uC7A1\uD55C\u2026"
lastmod: '2024-03-13T22:44:55.125010-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 \uC77C\uBD80 \uB2E4\uB978 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uB0A0\
  \uC9DC \uD30C\uC2F1\uC5D0 \uAC15\uB825\uD55C \uB0B4\uC7A5 \uAE30\uB2A5\uC744 \uAC00\
  \uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC73C\uBA70 \uC8FC\uB85C Javascript \uC0C1\uD638\
  \ \uC6B4\uC6A9\uC131 \uB610\uB294 \uB354 \uBCF5\uC7A1\uD55C \uC791\uC5C5\uC744 \uC704\
  \uD55C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
