---
title:                "Haskell: Yaml 사용하기"
simple_title:         "Yaml 사용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜 

Haskell을 사용하여 YAML 작업에 참여하는 이유는 무엇일까요? YAML은 데이터를 쉽게 읽고 사용할 수 있는 형식으로 만들어주는 유틸리티 형식입니다. 함수형 프로그래밍 언어인 Haskell을 사용하면 YAML 데이터를 손쉽게 다룰 수 있습니다.

## 어떻게

Haskell에서 YAML을 다루는 방법은 아래와 같습니다. 

```Haskell 
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml

-- YAML 파일 불러오기
loadYaml :: IO ()
loadYaml = do
  maybeYaml <- decodeFile "config.yaml" :: IO (Maybe YamlValue)
  case maybeYaml of
    Nothing -> putStrLn "Could not load YAML file."
    Just yaml -> do
      putStrLn "YAML 파일이 성공적으로 불러와졌습니다!"
      print yaml -- yaml 데이터 출력

-- YAML 데이터 생성하기
createYaml :: IO ()
createYaml = do
  let yamlData = object [ "name" .= "John", "age" .= (25 :: Int) ]
  encodeFile "info.yaml" yamlData -- yaml 파일로 데이터 쓰기
  putStrLn "YAML 데이터가 성공적으로 생성되었습니다!"
```

위 예시 코드에서 `loadYaml` 함수는 `config.yaml` 파일을 불러온 다음 파일의 내용을 화면에 출력합니다. `createYaml` 함수는 `info.yaml` 파일을 생성하고 `name`과 `age`라는 데이터를 파일에 쓴 뒤 화면에 성공 메시지를 출력합니다.

```
YAML 파일이 성공적으로 불러와졌습니다!
Object [(String "name",String "John"),(String "age",Number 25)]
YAML 데이터가 성공적으로 생성되었습니다!
```

## 심층 분석 

YAML 데이터는 널리 사용되는 설정 파일, 메타 데이터 및 마크업 언어로, 프로그래밍에서 자주 사용됩니다. Haskell에서는 데이터를 다루는 강력한 함수형 프로그래밍 기법을 적용하므로 YAML 데이터를 프로그램에서 처리하는 것이 더욱 간편해집니다.

예를 들어, `YAML 데이터 생성하기` 예시에서 `yamlData` 변수를 부분적으로 수정하여 다음과 같이 작성하면 쉽게 데이터를 수정할 수 있습니다.

```Haskell
let yamlData = merge ( object [ "name" .= "John" ] ) ( object [ "name" .= "Jane", "age" .= (30 :: Int) ] )
```

위 예시 코드는 `name`과 `age`라는 데이터를 수정하고 다음과 같은 결과를 출력합니다.

```
Object [(String "name",String "Jane"),(String "age",Number 30)]
```

더 많은 Haskell과 YAML에 대한 정보는 아래 링크들을 참고하시기 바랍니다.

# 참고 자료
- [Haskell에서 YAML 다루기](https://www.stackage.org/haddock/lts-16.27/yaml-0.11.5.0/Data-Yaml.html)
- [YAML 공식 사이트](https://yaml.org/)
- [Haskell 공식 사이트](https://www.haskell.org/)