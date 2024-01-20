---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 구성 파일, 메시지 교환과 같은 인간이 읽을 수 있는 데이터 직렬화 표준입니다. 프로그래머들은 설정, 프로젝트 메타데이터 관리, 데이터 저장 등을 위해 사용합니다.

## How to:

Haskell에서 YAML 작업을 시작하기 위해 `yaml` 패키지를 사용합시다. `stack install yaml`로 설치할 수 있습니다.

```Haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

-- YAML 데이터를 파싱합니다.
exampleYAML :: BS.ByteString
exampleYAML = "name: John Doe\nage: 30\n"

-- Haskell 타입으로 매핑하기 위한 사용자 정의 타입
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

instance FromJSON Person where
  parseJSON (Object v) = Person
    <$> v .: "name"
    <*> v .: "age"
  parseJSON _ = fail "Expected an object for Person"

main :: IO ()
main = do
  let personResult = decode exampleYAML :: Maybe Person
  case personResult of
    Just person -> print person
    Nothing -> putStrLn "Failed to parse YAML."
```

출력 예시:
```Haskell
Person {name = "John Doe", age = 30}
```

## Deep Dive

YAML(YAML Ain't Markup Language)은 2001년에 첫 출시되었으며 XML, JSON의 대안으로 개발되었습니다. 데이터를 트리 구조로 표현 가능하며, 주석과 보조 데이터 형식을 지원합니다.

YAML을 대처하는 다른 데이터 직렬화 포맷에는 JSON과 XML이 있습니다. 각각 성능, 전송 크기 등에서 장단점이 있습니다.

Haskell의 `yaml` 라이브러리는 libyaml C 라이브러리에 바인딩하여 성능을 향상시킵니다. `Data.Yaml` 모듈은 YAML 데이터를 쉽게 파싱 및 생성할 수 있는 인터페이스를 제공합니다.

## See Also

- YAML 공식 웹사이트: [https://yaml.org/](https://yaml.org/)
- yaml 라이브러리 Hackage 페이지: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- Aeson으로 JSON 처리 배우기: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)