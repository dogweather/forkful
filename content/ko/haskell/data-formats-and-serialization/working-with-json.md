---
title:                "JSON과 함께 일하기"
aliases:
- /ko/haskell/working-with-json.md
date:                  2024-02-03T19:23:06.474895-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Haskell에서 JSON(JavaScript Object Notation)을 다루는 일은 JSON 데이터를 Haskell 타입으로 파싱하는 것과 Haskell 타입을 다시 JSON으로 변환하는 것을 포함합니다. 프로그래머들은 Haskell 애플리케이션을 웹 서비스나 API와 원활하게 데이터를 교환할 수 있게 하기 위해 이러한 작업을 합니다. 이는 현대 소프트웨어 개발에서 플랫폼 간 데이터 교환을 위한 일반적인 관행입니다.

## 어떻게:
Haskell은 JavaScript와 같은 내장 JSON 지원이 없지만, **Aeson**과 같은 서드파티 라이브러리의 도움을 받아 JSON을 다루는 일이 간단해집니다. Aeson은 인코딩(Haskell 값들을 JSON으로 변환하는 것)과 디코딩(JSON을 Haskell 값으로 파싱하는 것)을 위한 고수준 및 저수준 함수를 제공합니다.

### Aeson 설치하기
먼저, 프로젝트의 의존성에 Aeson을 추가하기 위해 `.cabal` 파일을 업데이트하거나 Stack이나 Cabal을 직접 사용하세요:

```shell
cabal update && cabal install aeson
```
또는, Stack을 사용하는 경우:
```shell
stack install aeson
```

### JSON 파싱하기
JSON 데이터를 Haskell 타입으로 디코딩하는 기본 예를 시작해보겠습니다. 다음과 같은 사람을 나타내는 JSON을 가지고 있다고 가정합니다:

```json
{
  "name": "John Doe",
  "age": 30
}
```

먼저, 해당 Haskell 데이터 타입을 정의하고 `FromJSON`의 인스턴스로 만드세요:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- 파일에서 JSON 디코딩하는 함수
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
사용법:
위에 보여진 JSON 데이터를 담고 있는 `person.json`을 가정하여 실행하면:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
샘플 출력:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Haskell 값들을 JSON으로 인코딩하기
Haskell 값을 다시 JSON으로 변환하려면, 타입을 `ToJSON`의 인스턴스로 만들고 `encode`를 사용해야 합니다.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- 이전에 사용된 Person 타입을 가정

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
샘플 출력:
```json
{"name":"Jane Doe","age":32}
```

이 예제들은 Aeson을 사용하여 Haskell에서 JSON을 다루는 기본을 보여줍니다. Aeson은 맞춤 파싱 규칙, 복잡한 중첩된 JSON을 다루기 등, 다양한 필요와 시나리오에 적합한 것을 포함하여 훨씬 더 많은 것을 제공한다는 것을 기억하세요.
