---
title:                "YAML로 작업하기"
aliases:
- /ko/haskell/working-with-yaml.md
date:                  2024-02-03T19:25:44.423204-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML은 "YAML Ain't Markup Language"의 약자로, 모든 프로그래밍 언어에 사용할 수 있는 사람이 읽기 편한 데이터 직렬화 표준입니다. 프로그래머들은 그것의 가독성과 간단한 구조 때문에 종종 YAML을 구성 파일과 언어 간 데이터 교환에 사용합니다.

## 어떻게 하나:

Haskell은 YAML 처리를 위한 내장 지원이 없지만, `yaml`과 `aeson`과 같은 제3자 라이브러리를 사용하여 YAML 데이터를 파싱하고 생성할 수 있습니다. 여기 YAML 데이터를 시작하는 방법이 있습니다:

### YAML 읽기
우선, `yaml` 패키지를 프로젝트의 의존성에 추가하세요. 그런 다음, 다음 예제를 사용하여 간단한 YAML 문서를 파싱할 수 있습니다:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- 예시 YAML 데이터
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- YAML 문서와 일치하는 데이터 구조를 정의
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Error parsing YAML: " ++ show err
    Right person -> print person
```
위 코드의 예시 출력은 다음과 같을 것입니다:
```
Person {name = "John Doe", age = 30}
```

### YAML 쓰기
Haskell 데이터 구조에서 YAML을 생성하기 위해, 아래에 표시된 것처럼 `yaml` 패키지의 인코딩 기능을 사용할 수 있습니다:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- 이전 예제에서 사용된 Person 데이터 구조 이용

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
이 프로그램의 출력물은 YAML 형식의 문자열이 될 것입니다:
```
name: Jane Doe
age: 25
```

이 예제들은 Haskell에서 YAML을 작업하기 위한 출발점으로 사용되어야 합니다. 당신의 필요에 따라, 이러한 라이브러리들이 제공하는 더 발전된 기능과 옵션들을 탐색하고 싶을 수도 있습니다.
