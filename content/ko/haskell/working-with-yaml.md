---
title:                "yaml 작업하기"
html_title:           "Haskell: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML은 인간이 쉽게 읽고 쓸 수 있는 형식으로 데이터를 저장하는 데 유용합니다. 이것은 프로그래밍 언어에 의존하지 않는 데이터 포맷으로 다양한 용도로 사용될 수 있습니다.

## 사용 방법

```Haskell
-- YAML 라이브러리 가져오기
import qualified Data.Yaml as Y

-- 데이터를 YAML 형식으로 인코딩하기
let data = [1, 2, 3]
let encoded = Y.encode data

-- YAML 파일 읽어오기
let decoded = Y.decodeFile "data.yaml" :: IO (Maybe [Int])
-- 출력: Just [1, 2, 3]
```

## 깊게 들어가기

YAML은 인간의 논리적 생각을 단순한 형태로 표현할 수 있도록 설계되었습니다. 이는 일반 텍스트로 작성되어 편집기를 통해 손쉽게 편집할 수 있습니다. 또한 다른 데이터 형식과의 연동도 용이하며, 하나의 YAML 문서 안에 여러 개의 문서를 포함할 수 있다는 장점도 있습니다.

## 관련 자료

- [Yaml 라이브러리 문서](https://hackage.haskell.org/package/yaml)
- [YAML 사용 예시](https://www.parsonsmatthews.com/2016/03/25/writing-yaml-haskell-using-data-yaml/)