---
title:                "json 사용하기"
html_title:           "Haskell: json 사용하기"
simple_title:         "json 사용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

JSON(JavaScript Object Notation) 작업은 데이터를 저장, 전송, 분석하기 위해 사용하는 프로그래밍 작업입니다. 프로그래머들은 JSON을 사용하여 데이터를 구조화하고, 간단하게 표현하고, 다른 시스템으로 전송하기 쉽게 만듭니다.

## 방법:

아래 예시를 통해 JSON을 Haskell로 작업하는 방법을 알아보겠습니다. 

```Haskell
import Data.Aeson

-- 예제 데이터
data Person = Person { name :: String, age :: Int } deriving (Show)

-- Person 데이터를 JSON으로 옮기기
instance ToJSON Person where 
    toJSON (Person name age) = object ["name" .= name, "age" .= age]

-- JSON을 Person 데이터로 바꾸기
instance FromJSON Person where 
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"

-- 메인 함수
main = do
    let person = Person "Sunny" 26 -- 예제 Person 데이터
    let json = encode person -- Person 데이터를 JSON으로 변환
    putStrLn $ show json -- JSON 출력
```
결과:
```Haskell
"{\"name\":\"Sunny\",\"age\":26}"
```

## 딥 다이브:

JSON은 자바스크립트에서 사용하기 위해 개발되었지만, 현재는 다른 언어에서도 많이 사용됩니다. 일반적으로 XML보다 간단하고 가볍기 때문입니다. Haskell에서는 Data.Aeson 라이브러리를 사용하여 JSON을 작업할 수 있습니다. 또한, 다른 대안으로는 YAML이 있지만, YAML은 JSON보다 많은 기능을 제공하므로 더 복잡할 수 있습니다.

## 관련 자료:

- JSON 공식 웹사이트: https://www.json.org/
- Haskell Data.Aeson 라이브러리 문서: https://hackage.haskell.org/package/aeson
- YAML 공식 웹사이트: https://yaml.org/