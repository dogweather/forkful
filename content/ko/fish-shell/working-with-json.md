---
title:                "Fish Shell: json 처리하기"
simple_title:         "json 처리하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

# 왜 JSON 작업을 시작해야 하나요?

JSON은 사람과 컴퓨터 모두가 쉽게 읽고 쓸 수 있는 데이터 형식입니다. 이것은 널리 사용되는 웹 개발에서 매우 중요한 역할을 합니다. 만약 당신이 웹 개발자라면, JSON에 대한 이해는 필수적입니다.

## 작업하는 방법

JSON을 다루기 위해서는 Fish Shell에서 제공하는 몇 가지 유용한 명령어들을 알 필요가 있습니다.

### 객체 생성하기

Fish Shell에서 JSON 객체를 생성하는 방법은 매우 간단합니다.

```Fish Shell
set -g obj '{"name": "John", "age": 28, "city": "Seoul"}'
```

이렇게 변수에 JSON 객체를 생성하면, 해당 객체를 다양한 방법으로 활용할 수 있습니다.

### JSON 데이터 읽기

Fish Shell에서는 `jq`라는 명령어를 사용하여 JSON 데이터를 한눈에 볼 수 있습니다. 예를 들어 위에서 생성한 객체를 다음과 같이 읽을 수 있습니다.

```Fish Shell
echo $obj | jq
```

이렇게 하면 다음과 같은 결과가 출력됩니다.

```
{
  "name": "John",
  "age": 28,
  "city": "Seoul"
}
```

### 데이터 추가하기

Fish Shell에서는 `jq` 명령어를 통해 기존 JSON 객체에 새로운 데이터를 추가할 수 있습니다.

```Fish Shell
echo $obj | jq '. + {"hobby": "photography"}'
```

위와 같이 입력하면, 새로운 키와 값인 "hobby": "photography"가 추가된 결과가 출력됩니다.

```
{
  "name": "John",
  "age": 28,
  "city": "Seoul",
  "hobby": "photography"
}
```

이 밖에도 많은 기능들이 Fish Shell에서 제공되므로, JSON 작업에 필요한 다양한 기능들을 참고하시기 바랍니다.

## 더 깊게 들어가기

JSON 작업은 여러 가지 변환, 필터링, 검색 등 다양한 작업이 가능합니다. Fish Shell에서는 이를 위해 다양한 명령어들을 제공합니다. 예를 들어 `jq` 명령어를 사용하여, 조건에 따라 필터링하거나 원하는 데이터를 추출할 수 있습니다.

## 활용하는 다른 방법들

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [jq 공식 문서](https://stedolan.github.io/jq/)
- [JSON 공식 사이트](https://www.json.org/json-ko.html)

# 또 다른 정보 찾아보기

- [JSON 데이터 구조에서 읽어오기](https://www.json.org/json-ko.html)
- [Fish Shell의 다양한 명령어들](https://fishshell.com/docs/current/commands.html)
- [JSON에 대한 더 깊은 이해](https://www.w3schools.com/js/js_json_intro.asp)