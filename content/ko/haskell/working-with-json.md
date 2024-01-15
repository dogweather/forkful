---
title:                "JSON 작업하기"
html_title:           "Haskell: JSON 작업하기"
simple_title:         "JSON 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 현재 웹 프로그래밍에서 매우 중요한 데이터 형식입니다. 따라서 Haskell 프로그래머로서는 JSON을 다루는 능력이 필수적이며, 그것을 통해 웹 애플리케이션 개발에 더욱 강력한 영향을 미칠 수 있습니다.

## 어떻게

JSON은 Haskell에서 Data.Aeson 라이브러리를 이용하여 다룰 수 있습니다. 다음은 Data.Aeson 라이브러리를 이용하여 JSON 데이터를 읽고 쓰는 예시 코드입니다.

```Haskell
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

-- JSON 데이터를 읽어오는 함수
-- 바이트 스트링으로부터 JSON 데이터를 파싱하여 해당하는 자료형으로 반환
readJSON :: FromJSON a => BSL.ByteString -> Maybe a
readJSON = decode

-- 자료형을 JSON 데이터로 변환하는 함수
-- 주어진 자료형을 JSON 형식으로 인코딩하여 바이트 스트링으로 반환
writeJSON :: ToJSON a => a -> BSL.ByteString
writeJSON = encode

-- 예시 JSON 데이터
jsonString :: BSL.ByteString
jsonString = "{\"name\":\"John\", \"age\":30, \"language\":\"Haskell\"}"

-- 인코딩된 JSON 데이터를 자료형으로 파싱
main = case readJSON jsonString of
    Nothing -> putStrLn "JSON 파싱에 실패하였습니다."
    Just person -> putStrLn $ "안녕하세요, " ++ name person ++ "님! Haskell을 사용하는 것을 환영합니다!"

-- 자료형 정의
data Person = Person
    { name :: String
    , age :: Int
    , language :: String
    } deriving (Show, Generic) -- Generic을 사용하면 자동으로 인스턴스 생성 가능

-- JSON 파싱 규칙 정의
instance FromJSON Person
instance ToJSON Person
```

위 코드에서는 Data.Aeson 라이브러리의 decode와 encode 함수를 이용하여 간단하게 JSON 데이터를 파싱하고 인코딩하는 방법을 보여주었습니다.

## 더 깊이 들어가기

Data.Aeson 라이브러리에는 기본적인 JSON 데이터 형식 외에도 더 많은 기능들이 포함되어 있습니다. 예를 들어, JSON 항목의 값을 각각 다른 자료형으로 변환하는 기능이나 커스텀 타입을 JSON 데이터로 인코딩/디코딩하는 기능 등을 제공합니다. 또한, Haskell의 렌즈 라이브러리와 함께 사용하면 더욱 쉽게 JSON 데이터를 다룰 수 있습니다. 이러한 여러 기능들을 사용하여 좀 더 복잡한 JSON 데이터를 다룰 수 있으며, 개인적으로는 Haskell에서 JSON을 다루는 것이 다른 언어보다 더 우아하고 직관적이라고 생각합니다.

## 더 알아보기
* [Data.Aeson 라이브러리 문서](https://hackage.haskell.org/package/aeson)
* [Haskell 렌즈 라이브러리 문서](https://hackage.haskell.org/package/lens)
* [JSON 소개 및 기본 개념](https://www.json.org/json-ko.html)