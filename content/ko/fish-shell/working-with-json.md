---
title:                "json으로 작업하기"
html_title:           "Fish Shell: json으로 작업하기"
simple_title:         "json으로 작업하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON을 사용해서 프로그래밍을 하는 이유는 무엇일까요? JSON은 데이터를 구조화하고 전송하기에 효율적이며, 해당 포맷은 대부분의 언어에서 지원하므로 데이터 교환에 용이합니다.

## 코딩 양식

```Fish Shell``` 코드 블록 안에서 코딩 예시와 샘플 출력을 제공합니다. 우선, JSON을 파싱하는 방법을 알아보겠습니다.

```
set response (curl -s https://jsonplaceholder.typicode.com/posts)
echo $response | from-json
```

위의 코드 블록에서는 Fish Shell의 `curl` 명령어를 사용하여 URL에서 JSON 데이터를 가져오고, 해당 데이터를 `from-json` 함수를 통해 파싱합니다. 이후 해당 데이터를 출력해보면 다음과 같은 결과를 볼 수 있습니다.

```
[
  {
    "userId": 1,
    "id": 1,
    "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
    "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
  },
  {
    "userId": 1,
    "id": 2,
    "title": "qui est esse",
    "body": "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla"
  },
  ...
]
```

위와 같이 JSON 데이터를 파싱하면 각각의 객체를 배열로 구성하고 있음을 알 수 있습니다.

## 깊이 파고들기

JSON 데이터를 다루는 더 많은 방법과 기능은 [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)와 [JSON 파싱에 관한 블로그 포스트](https://devblogs.microsoft.com/scripting/json-parsing-made-easy/)를 참고하세요.

## 관련 자료

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [JSON 파싱에 관한 블로그 포스트](https://devblogs.microsoft.com/scripting/json-parsing-made-easy/)
- [JSON Formatter & Validator](https://jsonformatter.curiousconcept.com/)