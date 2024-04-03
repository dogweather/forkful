---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:06.474895-07:00
description: "\uC5B4\uB5BB\uAC8C: Haskell\uC740 JavaScript\uC640 \uAC19\uC740 \uB0B4\
  \uC7A5 JSON \uC9C0\uC6D0\uC774 \uC5C6\uC9C0\uB9CC, **Aeson**\uACFC \uAC19\uC740\
  \ \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uB3C4\uC6C0\uC744\
  \ \uBC1B\uC544 JSON\uC744 \uB2E4\uB8E8\uB294 \uC77C\uC774 \uAC04\uB2E8\uD574\uC9D1\
  \uB2C8\uB2E4. Aeson\uC740 \uC778\uCF54\uB529(Haskell \uAC12\uB4E4\uC744 JSON\uC73C\
  \uB85C \uBCC0\uD658\uD558\uB294 \uAC83)\uACFC \uB514\uCF54\uB529(JSON\uC744\u2026"
lastmod: '2024-03-13T22:44:55.326531-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC740 JavaScript\uC640 \uAC19\uC740 \uB0B4\uC7A5 JSON \uC9C0\uC6D0\
  \uC774 \uC5C6\uC9C0\uB9CC, **Aeson**\uACFC \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 \uB3C4\uC6C0\uC744 \uBC1B\uC544 JSON\uC744\
  \ \uB2E4\uB8E8\uB294 \uC77C\uC774 \uAC04\uB2E8\uD574\uC9D1\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

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
