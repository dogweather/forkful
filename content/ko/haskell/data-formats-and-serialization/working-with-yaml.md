---
aliases:
- /ko/haskell/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:44.423204-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uBAA8\uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uC0AC\uC6A9\uD560\
  \ \uC218 \uC788\uB294 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uD3B8\uD55C \uB370\uC774\uD130\
  \ \uC9C1\uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uADF8\uAC83\uC758 \uAC00\uB3C5\uC131\uACFC \uAC04\uB2E8\uD55C \uAD6C\
  \uC870 \uB54C\uBB38\uC5D0 \uC885\uC885 YAML\uC744 \uAD6C\uC131 \uD30C\uC77C\uACFC\
  \ \uC5B8\uC5B4 \uAC04 \uB370\uC774\uD130 \uAD50\uD658\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
lastmod: 2024-02-18 23:09:06.319092
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uBAA8\
  \uB4E0 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0 \uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uB294 \uC0AC\uB78C\uC774 \uC77D\uAE30 \uD3B8\uD55C \uB370\uC774\uD130 \uC9C1\
  \uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uADF8\uAC83\uC758 \uAC00\uB3C5\uC131\uACFC \uAC04\uB2E8\uD55C \uAD6C\uC870\
  \ \uB54C\uBB38\uC5D0 \uC885\uC885 YAML\uC744 \uAD6C\uC131 \uD30C\uC77C\uACFC \uC5B8\
  \uC5B4 \uAC04 \uB370\uC774\uD130 \uAD50\uD658\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
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
