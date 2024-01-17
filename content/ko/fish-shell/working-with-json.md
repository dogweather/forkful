---
title:                "json 데이터 다루기"
html_title:           "Fish Shell: json 데이터 다루기"
simple_title:         "json 데이터 다루기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

JSON을 다루는 것은 프로그래머들이 데이터를 효율적으로 저장하고 교환하기 위해 할 수 있는 방법입니다. 이것은 간단하고 가독성이 높은 데이터 포맷으로, 다양한 애플리케이션 간에 데이터를 쉽게 전송할 수 있도록 도와줍니다.

## 어떻게:

```Fish Shell```에서 JSON 데이터를 다루는 방법은 간단합니다. 먼저, JSON 데이터를 변수에 저장합니다.

```
set data "{'name': 'John', 'age': 25, 'city': 'Seoul'}"
```

저장한 데이터를 확인하기 위해 다음 명령어를 입력합니다.

```
echo $data
```

결과는 다음과 같습니다.

```
{'name': 'John', 'age': 25, 'city': 'Seoul'}
```

JSON 데이터에서 특정 값을 추출하려면 다음과 같이 변수를 사용합니다.

```
echo $data.name
```

결과는 다음과 같습니다.

```
John
```

더 복잡한 JSON 데이터를 핸들링하는 방법에 대해서는 이하 "깊게 파헤치기" 섹션에서 설명하겠습니다.

## 깊게 파헤치기:

JSON은 Lightweight markup 형식이며, 다양한 프로그래밍 언어에서 지원되고 있습니다. 하지만 가장 큰 차이점은 JSON 데이터를 파싱하는 방식입니다. 많은 언어에서는 자체적인 JSON 파싱 라이브러리를 제공하지만, Fish Shell에서는 이미 기본적으로 JSON 데이터를 다루는 기능을 제공합니다.

만약 Fish Shell에서 JSON 데이터를 더욱 쉽게 다루고 싶다면, 다음과 같은 추가 패키지를 사용할 수 있습니다.

- [fish-json](https://github.com/tuvistavie/fish-json): JSON 데이터를 파싱하고 다루는 데 유용한 기능을 제공합니다.

- [fish-jq](https://github.com/oh-my-fish/plugin-jq): jq 구문과 비슷한 구문을 사용해 JSON 데이터를 다룰 수 있도록 도와줍니다.

더 자세한 내용은 위의 링크를 참고하시기 바랍니다.

## 더 알아보기:

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/): Fish Shell의 공식 문서입니다. JSON 데이터를 다루는데 필요한 기능과 관련된 정보를 찾을 수 있습니다.

- [Learn X in Y minutes - JSON](https://learnxinyminutes.com/docs/json/): JSON 데이터 포맷을 학습하는데 도움이 되는 간단하고 빠른 가이드입니다.

- [JSONLint](https://jsonlint.com/): JSON 데이터의 문법을 검사하고 디버깅하는 데 유용한 온라인 도구입니다.