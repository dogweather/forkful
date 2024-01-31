---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
JSON은 데이터를 저장하고 전송하는 텍스트 기반 포맷입니다. 프로그래머들은 구조적이고 경량인 특성 덕분에 데이터 교환을 위해 JSON을 자주 사용합니다.

## How to: (어떻게 하나요?)
Haskell에서는 Aeson 라이브러리를 이용해 JSON을 다룹니다. 예제를 통해 사용법을 익혀봅시다.

```Haskell
import Data.Aeson

-- JSON 객체를 Haskell 값으로 변환
decodeJson :: ByteString -> Maybe Person
decodeJson json = decode json

-- Haskell 값에서 JSON 객체로 변환
encodeJson :: Person -> ByteString
encodeJson person = encode person

-- 예제 타입과 인스턴스
data Person = Person { name :: String, age :: Int } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

-- JSON 예제와 처리 결과
let personJson = "{\"name\":\"John\", \"age\":30}"
let person = decodeJson (Data.ByteString.Char8.pack personJson) :: Maybe Person

case person of
    Nothing -> putStrLn "Invalid JSON"
    Just p -> print p  -- 출력: Person {name = "John", age = 30}
```

## Deep Dive (심층 탐구)
JSON은 JavaScript에 기반을 둔 데이터 포맷으로, 2001년도에 도입되었습니다. Haskell에서는 Aeson 외에도 json, jsonb 등 다양한 라이브러리가 있지만 Aeson이 가장 인기가 많습니다. Aeson은 고성능을 위해 attoparsec라는 파서 콤비네이터 라이브러리를 사용합니다.

## See Also (참고 자료)
- Aeson GitHub 페이지: [https://github.com/haskell/aeson](https://github.com/haskell/aeson)
- JSON 공식 웹사이트: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Learn You a Haskell for Great Good!, JSON 챕터: [http://learnyouahaskell.com](http://learnyouahaskell.com)
