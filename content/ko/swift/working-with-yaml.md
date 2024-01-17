---
title:                "yaml 작업하기"
html_title:           "Swift: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## 뭔가 & 왜?

YAML 작업에 대해 짧게 설명하면, 이는 데이터 양식으로 개발자가 구조화 된 데이터를 저장할 수 있는 텍스트 형식이다. 개발자들은 이를 사용하여 간단하고 가독성이 높은 데이터 포멧을 생성할 수 있다.

## 어떻게 하면?

```Swift
let yamlString = """
name: John Doe
age: 30
occupation: Programmer
"""
print(yamlString)
```

```
name: John Doe
age: 30
occupation: Programmer
```

위와 같이 Swift에서는 간단한 문법을 사용하여 YAML 데이터를 만들 수 있다.

## 더 들어가보기

### 역사적 배경

YAML은 2001년에 최초 발표된 마크 스트립링이 설계한 데이터 표현 언어이다. 그 후 많은 프로그래밍 언어에서 YAML을 지원하며, 현재로서는 널리 사용되고 있다.

### 대안

YAML 외에도 프로그래머들은 JSON 혹은 XML과 같은 다른 데이터 포멧을 사용할 수 있다. 하지만 YAML은 다른 포멧보다 유연성과 가독성이 높아 사용이 용이하다.

### 구현 세부사항

Swift에서는 로컬 라이브러리인 Yams를 사용하여 YAML을 다룰 수 있다. open source 라이브러리이므로 github에서 쉽게 찾아볼 수 있다.

## 관련 링크

- [YAML 공식 웹사이트](https://yaml.org/)
- [Yams 라이브러리 github 페이지](https://github.com/jpsim/Yams)
- [Swift에서 YAML 사용하기](https://www.raywenderlich.com/1159761-yaml-tutorial-get-started-in-swift)